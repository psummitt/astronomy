# Implementation Plan - Radec Flutter App

The goal is to create a multi-platform Flutter application (Android, Linux, Web, Windows) that provides astronomical data for planets. The app will feature two modes: a local calculation based on `radec.bas` and a remote fetch from the NASA JPL Horizons API.

## User Review Required

- **Data Source**: I've chosen the NASA JPL Horizons API for the "internet" option as it provides high-precision data for any date.
- **Project Structure**: I will initialize the Flutter project in the current `radec` directory.

## Proposed Changes

### Project Initialization
- Initialize Flutter project with support for all requested platforms.
- Add necessary dependencies: `http`, `intl`, `provider` (for state management), and `url_launcher` (for links in history/instructions).

### [Component] Domain Models
#### [NEW] [planet_data.dart](file:///C:/Users/summi/GitHub/radec/lib/models/planet_data.dart)
Define a model to store Right Ascension, Declination, Distance to Sun, and Distance to Earth.

### [Component] Calculation Logic
#### [NEW] [local_calculator.dart](file:///C:/Users/summi/GitHub/radec/lib/services/local_calculator.dart)
Port the BASIC logic from `radec.bas` to Dart. This includes:
- Epoch 1960 date calculations.
- Heliocentric longitude and distance formulas.
- Coordinate transformations to RA and Dec.

#### [NEW] [remote_service.dart](file:///C:/Users/summi/GitHub/radec/lib/services/remote_service.dart)
Implement a service to fetch data from NASA JPL Horizons API.

### [Component] UI Layer
#### [NEW] [main.dart](file:///C:/Users/summi/GitHub/radec/lib/main.dart)
Entry point and theme setup (Material 3).

#### [NEW] [home_screen.dart](file:///C:/Users/summi/GitHub/radec/lib/ui/home_screen.dart)
Main navigation to Calculator, Instructions, and History.

#### [NEW] [calculator_screen.dart](file:///C:/Users/summi/GitHub/radec/lib/ui/calculator_screen.dart)
UI for selecting date and viewing results. Supports switching between Local and Remote modes.

#### [NEW] [instructions_screen.dart](file:///C:/Users/summi/GitHub/radec/lib/ui/instructions_screen.dart)
Informational page on how to use the app.

#### [NEW] [history_screen.dart](file:///C:/Users/summi/GitHub/radec/lib/ui/history_screen.dart)
Page detailing the history of the `radec.bas` program.

### [Component] Accessibility
- Use `Semantics` widgets for all interactive and informative elements.
- Ensure proper color contrast and text scaling support.
- Screen reader friendly table layouts for results.

## Verification Plan

### Automated Tests
- Unit tests for `LocalCalculator` comparing results with known values or the original logic.
- Mock tests for `RemoteService`.

### Manual Verification
- Run on Windows, Web, and Android (via emulator/device if available) to verify layout and accessibility.
- Check "History" and "Instructions" pages for readability.
