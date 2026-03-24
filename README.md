# Break down the size of a Scala Native binary


```
$ sn-sizer serve test-binary
Starting server on http://localhost:52616
```

When you navigate to the page, you will be greeted with an interactive dashboard allowing to drill down into the various packages and their sizes.

<img width="3752" height="1824" alt="CleanShot 2026-03-24 at 12 47 13@2x" src="https://github.com/user-attachments/assets/deb6ce5b-4959-4780-a4ab-7a26db44bd05" />

At the moment, only Mach-O binaries are supported – to support Linux we need to port [this PR](https://github.com/indoorvivants/macho-elf-coff-parser/pull/8/changes), to support Windows we need a brave volunteer.
