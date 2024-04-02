# 6CCS3PRJ Compiler Project

## Project Description

This project is a compiler for a simple functional programming language, with similar syntax to Scala.
The compiler is implemented in Scala and generates LLVM IR code.

## Dependencies

- Java 21
- Clang
- LLVM toolchain

Any other dependencies (such as Scala itself) is handled by the build tool.

If your editor supports the [Development Container Specification](https://containers.dev/) such as VSCode, you can use the included devcontainer to download the required dependencies in an isolated docker container.

## Running the project

The project can be run using the build tool `mill` without compiling the project into a jar file. Running the bootstrap script `./mill` will:
- Install the correct version of Scala and Mill if not already installed
- Compile the project

To run the project, use the following command:

```bash
./mill run <args>
``` 

Where `<args>` are the arguments to pass to the compiler. Available args are:
- `--help`: View all available subcommands and flags (including those not listed here)
- `print <file>`: Print the LLVM IR of the program in the file `<file>`
- `write <file>`: Write the LLVM IR of the program in the file `<file>` to a file
- `compile <file>`: Compile the program inside `<file>` into a binary



The file refers to the available test programs in the `resources` directory.

Alternatively, you can compile the project into a jar file and run it using the following commands:

```bash
./mill assembly
cd out/assembly.dest
java -jar out.jar <args>
```

## Testing

The project includes a test suite that can be run using the following command:

```bash
./mill test
```
