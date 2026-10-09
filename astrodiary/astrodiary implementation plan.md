# AstroDiary Implementation Plan

This document outlines the detailed architecture and implementation plan for creating the **AstroDiary** application inside the `astrodiary` directory based on the `Astrolog` application structure, supporting **Android**, **Linux**, **Web**, and **Windows** platforms.

> [!IMPORTANT]
> **Approval Required**: Do not proceed with code implementation until the user has reviewed and approved this implementation plan.

---

## 1. Overview & Objectives

1. **Multi-Platform Support**: Setup a complete Flutter application in `astrodiary/` supporting:
   - **Android** (`android/`)
   - **Linux** (`linux/`)
   - **Web** (`web/`)
   - **Windows** (`windows/`)
2. **Calendar-Like Presentation**:
   - Modify the application to save and display planned targets (`PlannerObservation`) and recorded observation logs (`ObservationLog`) in an interactive calendar view (`CalendarPage`).
   - Users can browse days on a monthly/weekly calendar grid, see visual indicators (badges/dots) for planned and completed observations, and tap any date to inspect, add, or manage entries for that day.
3. **Modern RA/DEC Calculation Integration for Planned Targets**:
   - **Moon**: If Target Name equals `"Moon"` (case-insensitive), calculate RA and Dec using the modern lunar position algorithm from the `radem` directory (`MoonCalculator`).
   - **Planets**: If Target Name equals one of the planets (*Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto*), calculate RA and Dec using the modern planetary calculation algorithm from the `radem`/`radec` directory (`PlanetCalculator`).
   - **Stars**: If Target Name equals a star name (e.g., *Sirius, Betelgeuse, Vega, Polaris, etc.*), calculate/fetch RA and Dec using CDS Strasbourg Sesame catalog resolution from the `starsearch` directory (`StarService`).

---

## 2. Technical Architecture & Component Breakdown

### A. Core Flutter Application Setup (`astrodiary/`)
- **`pubspec.yaml`**: Flutter package configuration with dependencies:
  - `flutter_riverpod` (state management)
  - `go_router` (declarative routing)
  - `table_calendar` (interactive calendar presentation)
  - `sqflite` & `sqflite_common_ffi` (SQLite database for local storage on mobile & desktop)
  - `firebase_core`, `firebase_auth`, `cloud_firestore` (cloud sync & authentication)
  - `http`, `xml` (SESAME API queries for star coordinate resolution)
  - `intl`, `path`, `path_provider`, `csv`, `file_picker`, `shared_preferences`
- **Platform Bundles**:
  - `android/` - Gradle Kotlin configuration with multi-dex & MinSDK 21+
  - `linux/` - CMake Linux desktop runner configuration
  - `web/` - CanvasKit / HTML Web runner configuration with `index.html` & manifest
  - `windows/` - Win32 CMake desktop runner configuration

### B. Navigation & Theme
- **Theme (`lib/app/theme.dart`)**: Light, Dark, and Red Night Vision mode (designed to preserve dark adaptation in field astronomy).
- **Router (`lib/app/router.dart`)**:
  - `/calendar` (Calendar-like presentation showing both planned and completed observations)
  - `/planner` (Planned observation list, CSV import/export, target planner)
  - `/planner/add` (Add plan with automatic RA/DEC modern calculator)
  - `/logbook` (Observation logbook list and log details)
  - `/logbook/add` & `/logbook/edit/:id` (Log entry forms)
  - `/settings`, `/login`, `/instructions`, `/about`
- **App Shell (`lib/app/app_shell.dart`)**: Responsive navigation bar (Mobile bottom navigation bar / Desktop navigation rail).

### C. Modern RA/DEC Astronomical Calculations
- **`lib/services/moon_calculator.dart`** (Ported from `radem/lib/moon_calculator.dart`):
  - `MoonCalculator._calculateModern(DateTime ut)`: Meeus-based high-accuracy lunar position model returning Right Ascension (hours) and Declination (degrees).
- **`lib/services/planet_calculator.dart`** (Ported from `radem`/`radec/lib/services/local_calculator.dart`):
  - `PlanetCalculator.calculate(String planetName, DateTime date)`: Keplerian planetary orbit propagation returning Right Ascension (hours) and Declination (degrees) for solar system planets.
- **`lib/services/star_service.dart`** (Ported from `starsearch/lib/services/star_service.dart`):
  - `StarService.fetchStarData(String starName)`: Asynchronous lookup querying CDS Strasbourg Sesame astronomical database returning J2000 RA and Dec.

### D. Calendar-Like Presentation (`lib/features/calendar/presentation/calendar_page.dart`)
- **Calendar Widget**: Monthly view with date selection, month navigation, and today shortcut.
- **Day Event Indicators**:
  - **Blue Dot**: Planned Observation(s) scheduled for that date.
  - **Green Dot**: Completed Observation Log(s) recorded on that date.
- **Selected Day Panel**:
  - Displays cards for planned targets and log entries for the selected day.
  - Quick action to "+ Plan Observation for Date" or "+ Log Observation for Date".
  - One-tap action to convert a planned observation directly into a logged observation upon completion.

---

## 3. Proposed File Changes & Hierarchy

```
astrodiary/
├── android/                   # Android build configuration
├── linux/                     # Linux desktop build configuration
├── web/                       # Web build configuration
├── windows/                   # Windows desktop build configuration
├── lib/
│   ├── app/
│   │   ├── app.dart
│   │   ├── app_shell.dart
│   │   ├── router.dart
│   │   └── theme.dart
│   ├── core/
│   │   ├── firebase_options.dart
│   │   ├── providers/
│   │   └── utils/
│   ├── features/
│   │   ├── calendar/          # [NEW] Calendar-like presentation feature
│   │   │   └── presentation/
│   │   │       └── calendar_page.dart
│   │   ├── logbook/           # Observation Logbook feature
│   │   │   ├── data/
│   │   │   ├── domain/
│   │   │   └── presentation/
│   │   ├── planner/           # Observation Planner feature
│   │   │   ├── data/
│   │   │   ├── domain/
│   │   │   └── presentation/
│   │   │       └── add_plan_page.dart  # [UPDATED] Auto-calculator integration
│   │   └── settings/
│   ├── services/              # [NEW] Astronomical Calculation Engines
│   │   ├── moon_calculator.dart        # From radem
│   │   ├── planet_calculator.dart      # From radem/radec
│   │   └── star_service.dart           # From starsearch
│   └── main.dart
├── pubspec.yaml
└── astrodiary implementation plan.md
```

---

## 4. Verification & Testing Plan

1. **Flutter Build Verification**:
   - Verify Flutter project compiles without errors (`flutter pub get`, `flutter analyze`).
   - Test platform builds for Linux and Web (`flutter build linux`, `flutter build web`).
2. **RA/DEC Calculation Accuracy Tests**:
   - Test target "Moon": verify RA/Dec is automatically populated with `MoonCalculator.calculate`.
   - Test target "Jupiter" / "Mars": verify RA/Dec is automatically calculated via `PlanetCalculator`.
   - Test target "Sirius" / "Betelgeuse": verify RA/Dec is fetched via `StarService`.
3. **Calendar Presentation Verification**:
   - Create a planned target for date $D$. Confirm blue indicator appears on $D$ in calendar.
   - Create an observed log for date $D$. Confirm green indicator appears on $D$ in calendar.
   - Select date $D$ in calendar and confirm both planned and observed entries appear in the day view panel.

---

> [!CAUTION]
> **Status**: Awaiting User Approval. Implementation will begin immediately upon your confirmation.
