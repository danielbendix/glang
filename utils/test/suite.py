import typer
import subprocess
import tempfile
import sys

from rich.console import Console, Group
from rich.table import Table
from rich.panel import Panel

from dataclasses import dataclass
from pathlib import Path
from expectations import Testcase, parse_file, ParserException


@dataclass
class Failure:
    header: str
    lines: list[str]

    def __init__(self, header: str, *lines: str):
        self.header = header
        self.lines = [line for line in lines]


def validate_test(
    input_file: Path,
    testcase_file: Testcase,
) -> list[str]:
    return []


def run_test(
    glang: Path,
    opt: str,
    clang: str,
    input_file: Path,
    testcase: Testcase,
    tmp_dir: Path,
) -> list[Failure] | None:
    file = Path(input_file)
    name = file.stem
    bitcode = tmp_dir / (name + ".bc")
    bitcode_opt = tmp_dir / (name + "_opt.bc")
    output = tmp_dir / (name + ".out")

    errors: list[Failure] = []

    glang_result = subprocess.run(
        [glang, file, "-o", bitcode], capture_output=True, text=True
    )

    # TODO: Support compile-failure outcome
    if glang_result.returncode != 0:
        errors.append(
            Failure(
                "Compilation error",
                f"'{glang}' exited with nonzero return code: {glang_result.returncode}",
            )
        )
        return errors

    if testcase.opt_flags is not None:
        opt_result = subprocess.run(
            [opt] + testcase.opt_flags + [bitcode, "-o", bitcode_opt],
            capture_output=True,
            text=True,
        )

        if opt_result.returncode != 0:
            errors.append(
                Failure(
                    "Error during LLVM optimization",
                    f"'{opt}' exited with nonzero return code: {opt_result.returncode}",
                )
            )
            return errors
    else:
        bitcode_opt = bitcode

    clang_result = subprocess.run(
        [clang, bitcode_opt, "-o", output], capture_output=True, text=True
    )

    if clang_result.returncode != 0:
        errors.append(
            Failure(
                "Linker error",
                f"'{clang}' exited with nonzero return code: {clang_result.returncode}",
            )
        )
        return errors

    program_result = subprocess.run([output], capture_output=True, text=True)

    # TODO: Support execute-failure outcome
    if program_result.returncode != 0:
        errors.append(
            Failure(
                "Runtime error",
                f"Compiled program failed during execution. Exit code: {program_result.returncode}.",
            )
        )

    if testcase.stdout is not None:
        if testcase.stdout.output != program_result.stdout:
            errors.append(
                Failure(
                    "stdout expectation failed",
                    "Expected: " + repr(testcase.stdout.output),
                    "Got:      " + repr(program_result.stdout),
                )
            )

    return errors


app = typer.Typer()
console = Console()


def load_testcase(code_file: Path, testcase_directory: Path) -> Testcase | None:
    name = code_file.stem
    testcase_file = testcase_directory / (name + ".gtest")
    try:
        return parse_file(testcase_file)
    except ParserException as parser_error:
        console.print(
            f"Unable to parse test case file '{testcase_file}': {parser_error}.",
            style="red",
        )
    except FileNotFoundError:
        console.print(
            f"Unable to open test case file '{testcase_file}'. File does not exist.",
            style="red",
        )
    return None


def load_testcases(code_files: list[Path], testcase_directory: Path) -> list[Testcase]:
    testcases: list[Testcase] = []

    for code_file in code_files:
        testcase = load_testcase(code_file, testcase_directory)
        if testcase is not None:
            testcases.append(testcase)
    if len(testcases) != len(code_files):
        sys.exit(1)
    return testcases


@app.command(help="Validate test suite")
def validate(directory: Path):
    testcase_directory = directory / "expected"

    code_files = list(directory.glob("*.gg"))
    code_files.sort()

    if len(code_files) == 0:
        console.print("No code files in test suite.", style="bold red")
        return

    testcases = load_testcases(code_files, testcase_directory)

    table = Table(title="Test cases", show_header=False)
    table.add_column()

    for code_file, testcase in zip(code_files, testcases):
        if testcase is None:
            return
        name = testcase.name if testcase.name is not None else code_file.stem
        table.add_row(name)
    console.print(table)


@app.command(help="Run test suite")
def run(directory: Path, glang: Path, clang: str, opt: str):
    testcase_directory = directory / "expected"

    code_files = list(directory.glob("*.gg"))
    code_files.sort()

    if len(code_files) == 0:
        console.print("No code files in test suite.", style="bold red")
        return

    testcases = load_testcases(code_files, testcase_directory)

    temporary_directory = tempfile.TemporaryDirectory()

    table = Table(title="Test suite result")
    table.add_column("Test case", style="white")
    table.add_column("Result", style="white")

    for code_file, testcase in zip(code_files, testcases):
        if testcase is None:
            return
        name = testcase.name if testcase.name is not None else code_file.stem
        errors = run_test(
            glang, opt, clang, code_file, testcase, Path(temporary_directory.name)
        )

        if not errors:
            table.add_row(name, "Succeeded", style="green")
        else:
            num_errors = len(errors)
            error_noun = "error" if num_errors == 1 else "errors"
            panels: list[Panel | str] = [f"Failed - {num_errors} {error_noun}"]
            for line in errors:
                panels.append(
                    Panel("\n".join(line.lines), title=line.header, border_style="red", style="white")
                )
            group = Group(*panels)
            table.add_row(name, group, style="red")

    console.print(table)


if __name__ == "__main__":
    app()
