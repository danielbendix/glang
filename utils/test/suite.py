import typer
import subprocess
import tempfile
import sys
import time

from rich.console import Console, Group
from rich.table import Table
from rich.panel import Panel

from dataclasses import dataclass, field
from pathlib import Path
from typing import Tuple
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


@dataclass
class RunResult:
    errors: list[Failure] = field(default_factory=lambda: [])
    glang_elapsed: float | None = None
    opt_elapsed: float | None = None
    clang_elapsed: float | None = None
    program_elapsed: float | None = None


def run_test(
    glang: Path,
    opt: str,
    clang: str,
    input_file: Path,
    testcase: Testcase,
    tmp_dir: Path,
) -> RunResult:
    def run_and_time_subprocess(
        args: list[str | Path],
    ) -> Tuple[float, subprocess.CompletedProcess[str]]:
        time_before = time.perf_counter()
        result = subprocess.run(args, capture_output=True, text=True)
        time_after = time.perf_counter()
        elapsed = time_after - time_before
        return (elapsed, result)

    file = Path(input_file)
    name = file.stem
    bitcode = tmp_dir / (name + ".bc")
    bitcode_opt = tmp_dir / (name + "_opt.bc")
    output = tmp_dir / (name + ".out")

    result = RunResult()

    (glang_elapsed, glang_result) = run_and_time_subprocess(
        [glang, file, "-o", bitcode]
    )
    result.glang_elapsed = glang_elapsed

    # TODO: Support compile-failure outcome
    if glang_result.returncode != 0:
        result.errors.append(
            Failure(
                "Compilation error",
                f"'{glang}' exited with nonzero return code: {glang_result.returncode}",
            )
        )
        return result

    if testcase.opt_flags is not None:
        (opt_elapsed, opt_result) = run_and_time_subprocess(
            [opt] + testcase.opt_flags + [bitcode, "-o", bitcode_opt],
        )
        result.opt_elapsed = opt_elapsed

        if opt_result.returncode != 0:
            result.errors.append(
                Failure(
                    "Error during LLVM optimization",
                    f"'{opt}' exited with nonzero return code: {opt_result.returncode}",
                )
            )
            return result
    else:
        bitcode_opt = bitcode

    (clang_elapsed, clang_result) = run_and_time_subprocess(
        [clang, bitcode_opt, "-o", output]
    )
    result.clang_elapsed = clang_elapsed

    if clang_result.returncode != 0:
        result.errors.append(
            Failure(
                "Linker error",
                f"'{clang}' exited with nonzero return code: {clang_result.returncode}",
            )
        )
        return result

    (program_elapsed, program_result) = run_and_time_subprocess([output])
    result.program_elapsed = program_elapsed

    # TODO: Support execute-failure outcome
    if program_result.returncode != 0:
        result.errors.append(
            Failure(
                "Runtime error",
                f"Compiled program failed during execution. Exit code: {program_result.returncode}.",
            )
        )

    if testcase.stdout is not None:
        if testcase.stdout.output != program_result.stdout:
            result.errors.append(
                Failure(
                    "stdout expectation failed",
                    "Expected: " + repr(testcase.stdout.output),
                    "Got:      " + repr(program_result.stdout),
                )
            )

    return result


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
def run(directory: Path, glang: Path, clang: str, opt: str, print_time: bool = False):
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

    if print_time:
        table.add_column("glang time", style="white")
        table.add_column("opt time", style="white")
        table.add_column("clang time", style="white")
        table.add_column("program time", style="white")

    for code_file, testcase in zip(code_files, testcases):
        if testcase is None:
            return
        name = testcase.name if testcase.name is not None else code_file.stem
        result = run_test(
            glang, opt, clang, code_file, testcase, Path(temporary_directory.name)
        )

        def format_time(time: float | None) -> str:
            if time is None:
                return "N/A"
            else:
                return f"{time:.3f}"

        if print_time:
            times = [
                format_time(time)
                for time in [
                    result.glang_elapsed,
                    result.opt_elapsed,
                    result.clang_elapsed,
                    result.program_elapsed,
                ]
            ]
        else:
            times = []

        if not result.errors:
            table.add_row(name, "Succeeded", *times, style="green")
        else:
            num_errors = len(result.errors)
            error_noun = "error" if num_errors == 1 else "errors"
            panels: list[Panel | str] = [f"Failed - {num_errors} {error_noun}"]
            for line in result.errors:
                panels.append(
                    Panel(
                        "\n".join(line.lines),
                        title=line.header,
                        border_style="red",
                        style="white",
                    )
                )
            group = Group(*panels)
            table.add_row(name, group, *times, style="red")

    console.print(table)


if __name__ == "__main__":
    app()
