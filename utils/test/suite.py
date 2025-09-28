import typer
import subprocess
import tempfile

from rich.console import Console
from pathlib import Path
from expectations import Testcase, parse_file, ParserException


def run_test(
    glang: Path,
    opt: str | None,
    clang: str,
    input_file: Path,
    expected_file: Path,
    tmp_dir: Path,
) -> list[str] | None:
    file = Path(input_file)
    name = file.stem
    bitcode = tmp_dir / (name + ".bc")
    bitcode_opt = tmp_dir / (name + "_opt.bc")
    output = tmp_dir / (name + ".out")

    error_lines: list[str] = []

    try: 
        testcase = parse_file(expected_file)
    except ParserException as parser_error:
        error_lines.append(f"Unable to parse test case file '{expected_file}': {parser_error}.")
        return error_lines
    except FileNotFoundError:
        error_lines.append(f"Unable to open test case file '{expected_file}'. File does not exist.")
        return error_lines

    glang_result = subprocess.run(
        [glang, file, "-o", bitcode], capture_output=True, text=True
    )

    # TODO: Support compile-failure outcome
    if glang_result.returncode != 0:
        error_lines.append(f"'{glang}' exited with nonzero return code: {glang_result.returncode}")
        return error_lines

    if testcase.opt_flags is not None:
        opt_result = subprocess.run(
            ["opt"] + testcase.opt_flags + [bitcode, "-o", bitcode_opt], capture_output=True, text=True
        )

        if opt_result.returncode != 0:
            error_lines.append(f"opt exited with nonzero return code: {opt_result.returncode}")
            return error_lines
    else:
        bitcode_opt = bitcode

    clang_result = subprocess.run(
        [clang, bitcode_opt, "-o", output], capture_output=True, text=True
    )

    if clang_result.returncode != 0:
        error_lines.append(f"'{clang}' exited with nonzero return code: {clang_result.returncode}")
        return error_lines

    program_result = subprocess.run([output], capture_output=True, text=True)

    # TODO: Support execute-failure outcome
    if program_result.returncode != 0:
        error_lines.append(f"Compiled program failed during execution. Exit code: {program_result.returncode}.")

    if testcase.stdout is not None:
        if testcase.stdout.output != program_result.stdout:
            error_lines.append("Did not get the expected output from stdout.")
            error_lines.append("Expected: " + repr(testcase.stdout.output))
            error_lines.append("Got:      " + repr(program_result.stdout))

    return error_lines


app = typer.Typer()

@app.command(help="Validate test suite")
def validate(directory: Path):
    pass

@app.command(help="Run test suite")
def run(directory: Path, glang: Path, clang: str, opt: str):
    expected_directory = directory / "expected"

    code_files = list(directory.glob("*.gg"))
    code_files.sort()

    temporary_directory = tempfile.TemporaryDirectory()

    console = Console()

    for code_file in code_files:
        name = code_file.stem
        expected_file = expected_directory / (name + ".gtest")
        result = run_test(glang, opt, clang, code_file, expected_file, Path(temporary_directory.name))
        if result:
            console.print(f"Test {name} failed:", style="bold red")
            for line in result:
                console.print(line, style="red")
        else:
            console.print(f"Test {name} succeeded", style="green")




if __name__ == "__main__":
    app()
