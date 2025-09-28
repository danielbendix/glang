import typer
import subprocess
import tempfile
import sys

from rich.console import Console
from pathlib import Path
from expectations import Testcase, parse_file, ParserException


def validate_test(
    input_file: Path,
    testcase_file: Testcase,
) -> list[str]:
    return []


def run_test(
    glang: Path,
    opt: str | None,
    clang: str,
    input_file: Path,
    testcase: Testcase,
    tmp_dir: Path,
) -> list[str] | None:
    file = Path(input_file)
    name = file.stem
    bitcode = tmp_dir / (name + ".bc")
    bitcode_opt = tmp_dir / (name + "_opt.bc")
    output = tmp_dir / (name + ".out")

    error_lines: list[str] = []

    glang_result = subprocess.run(
        [glang, file, "-o", bitcode], capture_output=True, text=True
    )

    # TODO: Support compile-failure outcome
    if glang_result.returncode != 0:
        error_lines.append(
            f"'{glang}' exited with nonzero return code: {glang_result.returncode}"
        )
        return error_lines

    if testcase.opt_flags is not None:
        opt_result = subprocess.run(
            ["opt"] + testcase.opt_flags + [bitcode, "-o", bitcode_opt],
            capture_output=True,
            text=True,
        )

        if opt_result.returncode != 0:
            error_lines.append(
                f"opt exited with nonzero return code: {opt_result.returncode}"
            )
            return error_lines
    else:
        bitcode_opt = bitcode

    clang_result = subprocess.run(
        [clang, bitcode_opt, "-o", output], capture_output=True, text=True
    )

    if clang_result.returncode != 0:
        error_lines.append(
            f"'{clang}' exited with nonzero return code: {clang_result.returncode}"
        )
        return error_lines

    program_result = subprocess.run([output], capture_output=True, text=True)

    # TODO: Support execute-failure outcome
    if program_result.returncode != 0:
        error_lines.append(
            f"Compiled program failed during execution. Exit code: {program_result.returncode}."
        )

    if testcase.stdout is not None:
        if testcase.stdout.output != program_result.stdout:
            error_lines.append("Did not get the expected output from stdout.")
            error_lines.append("Expected: " + repr(testcase.stdout.output))
            error_lines.append("Got:      " + repr(program_result.stdout))

    return error_lines


app = typer.Typer()
console = Console()


@app.command(help="Validate test suite")
def validate(directory: Path):
    pass


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

    for code_file, testcase in zip(code_files, testcases):
        if testcase is None:
            return
        name = testcase.name if testcase.name is not None else code_file.stem
        errors = run_test(
            glang, opt, clang, code_file, testcase, Path(temporary_directory.name)
        )

        if not errors:
            console.print(f"Test case '{name}' succeeded", style="green")
        else:
            console.print(f"Test case '{name}' failed:", style="bold red")
            for line in errors:
                console.print(line, style="red")


if __name__ == "__main__":
    app()
