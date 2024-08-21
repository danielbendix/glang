from pygls.server import LanguageServer
from lsprotocol.types import (
    Diagnostic,
    DiagnosticSeverity,
    Range,
    Position,
    TEXT_DOCUMENT_DID_SAVE,
    DidSaveTextDocumentParams,
    TEXT_DOCUMENT_DID_OPEN,
    DidOpenTextDocumentParams,
    MessageType,
)

import sys
import subprocess
import json


class GlanguageServer(LanguageServer):
    executable: str

    def __init__(self, executable: str):
        super().__init__("glang-lsp", "0.1.0")
        self.executable = executable


if len(sys.argv) == 2:
    executable = sys.argv[1]
else:
    executable = "glang"

server = GlanguageServer(executable)


def create_diagnostic(diagnostic: dict) -> Diagnostic:
    location: dict = diagnostic["location"]

    line: int = location["line"]
    column: int = location["column"]
    length: int = location["length"]

    start = Position(line=line - 1, character=column)
    end = Position(line=line - 1, character=column + length)
    range = Range(start=start, end=end)

    message: str = diagnostic["message"]

    match diagnostic["kind"]:
        case "error":
            severity = DiagnosticSeverity.Error
        case "warning":
            severity = DiagnosticSeverity.Warning
        case "note":
            severity = DiagnosticSeverity.Hint
        case _:
            raise Exception("Uknown kind.")

    return Diagnostic(
        range=range,
        message=message,
        severity=severity,
    )


def check_file(
    ls: GlanguageServer, params: DidOpenTextDocumentParams | DidSaveTextDocumentParams
):
    file_uri = params.text_document.uri
    file_path = file_uri.removeprefix("file://")

    process = subprocess.Popen(
        [
            ls.executable,
            "--validate",
            "--json",
            file_path,
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )

    stdout, stderr = process.communicate()

    if stderr:
        ls.show_message_log(f"Error: {stderr}", msg_type=MessageType.Error)
        return

    diagnostics: list[Diagnostic] = []
    ls.show_message_log(f"STDOUT: {stdout}")
    for line in stdout.splitlines():
        try:
            data = json.loads(line)

            diagnostics.append(create_diagnostic(data))

            ls.show_message_log(f"Processed JSON: {data}")
        except json.JSONDecodeError:
            ls.show_message_log(f"Invalid JSON: {line}", msg_type=MessageType.Error)

    server.publish_diagnostics(file_uri, diagnostics)


@server.feature(TEXT_DOCUMENT_DID_OPEN)
def did_open(ls: GlanguageServer, params: DidOpenTextDocumentParams):
    check_file(ls, params)


@server.feature(TEXT_DOCUMENT_DID_SAVE)
def did_save(ls: GlanguageServer, params: DidSaveTextDocumentParams):
    return check_file(ls, params)


if __name__ == "__main__":
    print("Starting")
    server.start_io()
