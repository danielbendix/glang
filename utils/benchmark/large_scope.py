import random
import string
import typer
from typing import Annotated, Optional


def generate_unique_identifiers(length, count) -> list[str]:
    if length <= 1 or count <= 0:
        raise ValueError(
            "Length must be greater than 1 and count must be a positive integer."
        )

    characters = string.ascii_letters + string.digits
    non_digit_characters = string.ascii_letters

    if count > len(non_digit_characters) * (len(characters) ** (length - 1)):
        raise ValueError(
            f"Unable to generate {count} unique identifiers with length {length}."
        )

    identifiers: set[str] = set()

    while len(identifiers) < count:
        identifier = random.choice(non_digit_characters) + "".join(
            random.choice(characters) for _ in range(length - 1)
        )
        identifiers.add(identifier)

    results = sorted(identifiers)
    random.shuffle(results)
    return results


def create_function(name: str) -> str:
    return f"fn {name}() -> i64 {{ return 123; }}"


def create_binding(variable: str, function: str) -> str:
    return f"    const {variable} = {function}();"


def create_return_value(variables: list[str]) -> str:
    def chunks(array, chunk_size):
        for i in range(0, len(array), chunk_size):
            yield array[i : i + chunk_size]

    def chunkify(array: list[str], chunk_size: int):
        while len(array) > 1:
            array = ['(' +  " + ".join(c) + ')' for c in chunks(array, chunk_size)]
        return array[0]

    terms = chunkify(variables, 100)

    #adds = ["result = result + " + " + ".join(c) + ";" for c in chunks(variables, 100)]

    return f"    return {terms};\n"
    #return f"    const result: i64 = 0;\n{"\n".join(adds)}\n    return result;"
    

def create_program(function_names: list[str], variables: list[str]) -> str:
    functions = [create_function(f) for f in function_names]

    bindings = [create_binding(v, f) for (v, f) in zip(variables, function_names)]

    return (
        "\n".join(functions)
        + "\nfn main() -> i64 {\n"
        + "\n".join(bindings)
        + "\n"
        + create_return_value(variables)
        + "\n}"
    )


app = typer.Typer(add_completion=False)


@app.command(
    help="Create a program which will create a given number of functions, and assign them to as many variables inside the main function."
)
def main(
    variables: Annotated[
        int,
        typer.Option(
            help="Number of variables to create in main function, as well as number of functions to create."
        ),
    ] = 40000,
    identifier_length: Annotated[
        int, typer.Option(help="Length of variable and function names")
    ] = 15,
    seed: Annotated[
        Optional[str], typer.Option(help="A hex string to use as a seed for the random number generator")
    ] = None,
):
    if seed is not None:
        seed_value = int(seed, base=16)
        random.seed(seed_value)

    identifiers = generate_unique_identifiers(identifier_length, variables * 2)
    print(create_program(identifiers[:variables], identifiers[variables:]))


if __name__ == "__main__":
    app()
