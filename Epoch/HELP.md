# Epoch I - Precession Calculator Help

## Overview
Epoch I is an astronomy utility designed to compute the Right Ascension (R.A.) and Declination (Dec.) of a celestial object for a specific year (epoch), based on its coordinates from another year. It accounts for the Earth's axial precession over time.

This program is a modern implementation of the original "Epoch I" BASIC program by Eric Burgess, F.R.A.S.

## How to Use

### 1. Set the Epochs
*   **First Epoch (Year):** Enter the year corresponding to your current coordinates (e.g., `1950.0` or `2000.0`).
*   **Second Epoch (Year):** Enter the target year you wish to calculate coordinates for (e.g., `2025.0`).

### 2. Enter Current Coordinates
Input the coordinates as they are recorded for the **First Epoch**:

*   **R.A. (Right Ascension):**
    *   **HR:** Hours (0-23)
    *   **MI:** Minutes (0-59)
    *   **SE:** Seconds (0-59)
*   **Dec. (Declination):**
    *   **DEG:** Degrees (-90 to +90)
    *   **MI:** Minutes (0-59)
    *   **SE:** Seconds (0-59)

### 3. Calculate
Click the **CALCULATE PRECESSION** button.

### 4. Read Results
The results will appear at the bottom of the screen, showing:
*   The target Epoch.
*   **Right Ascension:** Displayed in decimal hours and in Hours, Minutes, Seconds format.
*   **Declination:** Displayed in decimal degrees and in Degrees, Minutes, Seconds format.

## Note on Accuracy
The precession constants used in this program ($m = 3.07234$ and $n = 20.0468$) and the formula are based on the original 20th-century astronomical algorithms. While highly accurate for general use, very precise modern applications may require more complex IAU models.
