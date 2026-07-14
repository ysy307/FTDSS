# FTCMS Distribution

This archive contains a Release build of FTCMS and the non-system runtime
libraries required by the executable.

Set `FTCMS_PROJECT_PATH` to a project directory that contains
`Input/Basic.json`, `Input/Conditions.json`, and `Input/Output.json`.

Linux:

```bash
FTCMS_PROJECT_PATH=/path/to/project ./bin/FTCMS
```

Windows Command Prompt:

```bat
set FTCMS_PROJECT_PATH=C:\path\to\project
bin\FTCMS.exe
```

The Linux package includes MPI support and can run as one process without a
launcher. Running multiple Linux processes requires a compatible Intel MPI
launcher on the target system. The Windows package is a single-process build
without MPI or ScaLAPACK support.
