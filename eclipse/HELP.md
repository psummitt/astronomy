# Lunar Eclipse Calculator - Help Guide

## Overview
This application provides the dates and magnitudes of lunar umbral eclipses starting from a user-specified year. It supports both legacy 1980s logic and modern high-precision calculations.

## Getting Started
1. **Enter Year**: Type the year you wish to start searching from (e.g., 2024).
2. **Choose Engine**:
   - **Modern (Meeus)**: Recommended for accurate real-world observations.
   - **Legacy (Burgess)**: Use to see the output of the original 1980 BASIC program.
3. **View Results**: The app will display the next 10 umbral eclipses.

## Calculation Differences
The **Legacy** logic was written for home computers with limited precision and uses simplified astronomical formulas. It assumes a fixed rate of Earth's rotation and simplified lunar orbit parameters. You may notice it predicts eclipses slightly differently than modern data.

The **Modern** logic accounts for periodic perturbations in the Moon's orbit, variations in Delta-T, and precise solar coordinates.

## Accessibility Features
- **Screen Reader Support**: All elements are tagged for TalkBack, VoiceOver, and Narrator.
- **Dynamic Type**: Supports system-level font size adjustments.
- **Keyboard Navigation**: Fully navigable via Tab and Enter keys on Windows and Web.

## About the Author
Original BASIC logic by **Eric Burgess, F.R.A.S.**
Reimagined for Flutter to preserve astronomical computing history.
