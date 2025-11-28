from dataclasses import dataclass
from pathlib import Path
from enum import StrEnum


@dataclass
class ParserException(Exception):
    line: int
    offset: int
    message: str


@dataclass
class OutputExpectation:
    output: str


@dataclass
class DiagnosticExpectation:
    """We expect exactly diagnostics like the ones in this object."""

    strict: bool


class Outcome(StrEnum):
    SUCCESS = "success"
    COMPILE_FAILURE = "compile-failure"
    EXECUTE_FAILURE = "execute-failure"

    @classmethod
    def from_string(cls, string: str):
        try:
            return cls(string)
        except ValueError:
            return None


@dataclass
class Testcase:
    outcome: Outcome
    name: str | None = None
    compiler_flags: list[str] | None = None
    opt_flags: list[str] | None = None
    stdout: OutputExpectation | None = None
    stderr: OutputExpectation | None = None
    """None means that we expect zero diagnostics during compilation."""
    diagnostics: DiagnosticExpectation | None = None


@dataclass
class Section:
    name: str
    line_number: int
    lines: list[str]


class LineReader:
    def __init__(self, lines: list[str]):
        self.lines = list(reversed(lines))
        self.line = 1

    def has_more(self) -> bool:
        return len(self.lines) > 0

    def has_section(self) -> bool:
        return self.lines[-1].startswith("###")

    def _pop(self) -> str:
        if len(self.lines) == 0:
            raise ParserException(self.line, 0, "Cannot pop line.")
        self.line += 1
        return self.lines.pop()

    def read_section(self) -> Section:
        line = self._pop()
        if not line.startswith("###"):
            raise ParserException(
                self.line - 1, 0, "Section line must start with '###'"
            )
        line = line.removeprefix("###")
        line = line.strip()
        return Section(line, self.line - 1, [])

    def read_line(self) -> str:
        line = self._pop()
        if line.startswith("###"):
            raise ParserException(
                self.line - 1, 0, "Section line must start with '###'"
            )
        return line


def parse_into_sections(lines: list[str]) -> list[Section]:
    reader = LineReader(lines)

    if not reader.has_section():
        raise ParserException(
            reader.line, 0, "Test case file must start with a section"
        )

    sections: list[Section] = [reader.read_section()]

    while reader.has_more():
        if reader.has_section():
            sections.append(reader.read_section())
        else:
            sections[-1].lines.append(reader.read_line())

    return sections


def key_value_from_line(line: str) -> tuple[str, str]:
    split = line.split(":", maxsplit=1)
    if len(split) != 2:
        raise ParserException(-1, -1, f"Invalid key value pair: '{line.strip()}'.")
    [key, value] = split
    key = key.strip()
    value = value.strip()
    return (key, value)


def key_values_from_section(section: Section) -> dict[str, str]:
    result: dict[str, str] = {}
    for i, line in enumerate(section.lines):
        (key, value) = key_value_from_line(line)
        if result.get(key) is not None:
            raise ParserException(
                section.line_number + i, 0, f"Duplicate key '{key}' in section"
            )
        result[key] = value
    return result


def parse_optional_flags(
    dictionary: dict[str, str], key: str, command_name: str
) -> list[str] | None:
    disallowed_flags: set[str] = {"-o"}

    flags_string = dictionary.get(key)
    if flags_string is None:
        return None

    flags = flags_string.split(" ")

    for flag in flags:
        if flag in disallowed_flags:
            raise ParserException(
                -1, -1, f"Disallowed flag '{flag}' for {command_name}"
            )

    return flags


def parse_header_section(section: Section) -> Testcase:
    if section.name != "TESTCASE":
        ParserException(
            section.line_number, 0, "First section must be of type 'TESTCASE'"
        )

    known_keys: set[str] = {"outcome", "name", "compiler-flags", "opt"}
    key_values = key_values_from_section(section)

    outcome: Outcome | None = None
    outcome_string = key_values.get("outcome")
    if outcome_string is None:
        raise ParserException(-1, -1, f"No outcome specified for test case.")
    outcome = Outcome.from_string(outcome_string)
    if outcome is None:
        raise ParserException(-1, -1, f"Invalid outcome string '{outcome_string}'")

    name = key_values.get("name")

    compiler_flags = parse_optional_flags(key_values, "compiler-flags", "glang")

    opt_flags = parse_optional_flags(key_values, "opt", "opt")

    for key in key_values.keys():
        if key not in known_keys:
            raise ParserException(-1, -1, f"Unknown test case key '{key}'")

    return Testcase(
        outcome=outcome,
        name=name,
        compiler_flags=compiler_flags,
        opt_flags=opt_flags,
    )


def parse(lines: list[str]) -> Testcase:
    sections = parse_into_sections(lines)

    test_case = parse_header_section(sections[0])

    for section in sections[1:]:
        match section.name:
            case "STDOUT":
                if test_case.stdout is not None:
                    raise ParserException(
                        section.line_number, 0, "Duplicate 'STDOUT' section."
                    )
                test_case.stdout = OutputExpectation("".join(section.lines))
            case "STDERR":
                if test_case.stderr is not None:
                    raise ParserException(
                        section.line_number, 0, "Duplicate 'STDERR' section."
                    )
                test_case.stderr = OutputExpectation("".join(section.lines))
            case "DIAGNOSTIC":
                raise ParserException(
                    section.line_number, 0, "'DIAGNOSTIC' is not yet implemented."
                )

    return test_case


def parse_file(path: Path) -> Testcase:
    with open(path, "r") as file:
        lines = file.readlines()
        return parse(lines)
