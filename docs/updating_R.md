# Updating R for `sadpaR` on Windows

`sadpaR` is a Shiny App written in R for Linux. Windows users can run `sadpaR` through a Shiny Desktop App bundled using [Shiny_Desktop_App](https://github.com/derryleng/Shiny_Desktop_App). This application _contains a full installation of R in portable mode_.

Sometimes, you may want to upgrade R within your shiny application.

## Steps to Update R

1. **Rename the Existing R Directory**
   - Navigate to the directory where your Shiny Desktop App is located.
   - Find the `R` directory within the app's folder.
   - Rename the `R` directory to `R.old` (or another suitable name).

2. **Download the Latest R Version**
   - Go to the [CRAN R Project website](https://cran.r-project.org/bin/windows/base/).
   - Download the latest R installer for Windows.

3. **Install R into the App's Directory**
   - Run the downloaded R installer.
   - During the installation process, choose the `R` directory within your Shiny Desktop App as the installation location.
     - **Note:** Ensure you specify the exact directory path to replace the old `R` directory (which was renamed).

     ```
     # Example: During installation, specify the path
     # C:\Path\To\Shiny_Desktop_App\R
     ```

4. **Verify the Installation**
   - After the installation is complete, ensure the new `R` directory contains the updated R files.
   - Check if the Shiny Desktop App runs correctly with the updated R version by running `run.bat` or `run_dataset_creator.bat`.

5. **Optional: Remove the Old R Directory**
   - If the application works correctly with the new R version, you may remove the `R.old` directory to free up space.

## Troubleshooting

- If you encounter issues after updating R, verify the installation path. There should always be an R directory within the Shiny_Desktop_App directory. If that is not the case, you probably specified a wrong installation path during installation of R.
- If you can't figure out what went wrong, you have the option of renaming `R.old` back to `R` and the application should work again.
- Refer to the [Shiny_Desktop_App GitHub repository](https://github.com/derryleng/Shiny_Desktop_App) for additional support and documentation.
