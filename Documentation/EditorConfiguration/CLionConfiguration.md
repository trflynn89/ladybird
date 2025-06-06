## CLion Project Configuration

CLion can integrate with CMake to provide code comprehension features.

After opening the `ladybird` repository in CLion as a new project, the "`Open Project Wizard`" window will open.

Select the `default` Preset in the `Settings -> Build, Execution and Deployment -> CMake` window and check the `Enable profile` checkbox.
Then select the `Debug` profile and uncheck the `Enable profile` checkbox.

If the build complains that there is no `Default` Toolchain, go to the `Settings -> Build, Execution and Deployment -> Toolchains`
tab and copy the currently defaulted host toolchain and rename it to `Default`. Make sure that the compiler chosen has a version of
supported by the [build system](../BuildInstructionsLadybird.md#build-prerequisites).

## Excluding Build Artifacts

Source files are copied to the `Build` directory during the build, if you do not exclude them from CLion indexing they will show up
in search results. This is often confusing, unintuitive, and can result in you losing changes you have made to files. To exclude
these files navigate to the `Project` tool window, right-click the `Build` folder and select `Mark Directory as | Excluded`.

## Include headers and source files for code insight

To get proper code insight mark the folders `AK` and `Libraries` by right-clicking on them and selecting `Mark Directory as | Project Sources and Headers`.

A symptom of this not being configured correctly is CLion giving a warning for every single file:
> The file does not belong to any project target, code insight features might not work properly.

## Code Generation Settings

To make code generated by CLion match the Ladybird coding style, import the `CLionCodeStyleSettings.xml` from this directory as code style scheme via
`Settings -> Editor -> Code Style -> C/C++ -> Scheme -> Cog icon -> Import Scheme...`

## CMake Error Messages

This section outlines common CMake error messages and provides guidance on how to resolve them.

### Unable to Find Package

You may encounter an error message similar to the following:

```
CMake Error at AK/CMakeLists.txt:60 (find_package):
By not providing "Findsimdutf.cmake" in CMAKE_MODULE_PATH, this project has
requested CMake to find a package configuration file for "simdutf", but
CMake could not locate one.
...
```

This error typically arises when CLion is not configured to use the correct build directory.

**Solution**: Ensure that CLion's build directory is set to the correct build directory for the selected profile.
Navigate to `Settings -> Build, Execution, Deployment -> CMake` and in your selected profile, set the `Build directory` according to the profile:
- Default -> "`Build/release`"
- Debug -> "`Build/debug`"
- Sanitizer -> "`Build/sanitizers`"


## Notes for WSL Users

### Toolchain

If the ladybird directory is on the WSL filesystem you need to configure the CLion toolchain to be WSL.
To set that up go to `File->Settings->Build, Execution, Deployment->Toolchains` and click on the `+` icon, then select WSL. In `Toolset` select the distribution you have the ladybird directory on.

### Terminal

It is possible to set the embedded terminal in CLion to the one that your WSL distribution provides.
This way you can build and run ladybird without leaving the IDE.
Note that following will only help if you don't use an X-window server to access qemu.
It is possible to install qemu natively on Windows and allow WSL to use it instead of installing qemu first on (wsl) linux and then use X server to launch ladybird inside of it.
Check the updated manual [here](../BuildInstructionsLadybird.md#windows).

- Locate the terminal emulator for your linux distribution.
Open CMD with elevated privileges and cd to `C:/Program Files/WindowsApps/`.
The directory is usually hidden and requires additional privileges. You should be able to cd as administrator.
`dir` and look for your distribution in directory names. In case of Ubuntu, it starts with `CanonicalGroupLimited.Ubuntu20.04onWindows_2004.2020.424.0_x64`.
cd to it. The directory should contain the shell executable. In my case it's named `ubuntu2004.exe`.
Copy `absolute/path/to/ubuntu2004.exe`.

- Go to your IDE settings: `File->Settings->Tools->Terminal` and paste the path you just copied to `shell path`. Click OK.

- Close CLion and restart.

The default IDE terminal should now be changed to WSL, and now you can run `CLion/run.sh`.
You may also want to copy `ladybird/Meta/CLion/run.sh` to your project directory and run it from there, so that you don't have to fight with git every time you modify the script.
