# JS-Interpreter

A simple JavaScript interpreter written in OCaml.

This project is meant for learning and experimenting with interpreters.  
It supports a small JavaScript subset and runs from the command line.

## How to use

1. **Clone and build:**
    ```sh
    git clone https://github.com/Stupu-Eduard/JS-Interpreter.git
    cd JS-Interpreter
    ocamlbuild interpretor.native
    ```

2. **Run the interpreter**
    ```sh
    ./interpretor.native
    ```
    Or run it with a file:
    ```sh
    ./interpretor.native myscript.js
    ```

## Notes

- All the logic is in OCaml source files. Documentation is in TeX.
- This is a study/demonstration project.  
  Feel free to explore, use, fork or improve!

---

**License:** MIT — see [LICENSE](LICENSE).

**Author:** [Stupu-Eduard](https://github.com/Stupu-Eduard)
