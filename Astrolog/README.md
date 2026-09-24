# AstroLog

A unified, cross-platform astronomical observation planner and observation logbook built with **Flutter**. AstroLog brings together target planning (with celestial coordinates, scheduling, local SQLite persistence, and CSV import/export) and observation session logging (with equipment, weather, notes, Cloud Firestore sync, and multi-provider authentication) into a single accessible app targeting **Android**, **Linux Desktop**, and **Web**.

---

## 🌟 Key Features

### 📅 Astronomy Planner
- **Target Planning**: Plan upcoming observation targets with object names, target date/time, Right Ascension (RA), Declination (Dec), and session notes.
- **Local Persistence**: Full offline capability powered by **SQLite** (`sqflite` / `sqflite_common_ffi`) on Linux & Android, and web local storage on Web.
- **CSV Data Exchange**: Export observation plans to standard CSV format and import existing target lists from CSV files.

### 📓 Observation Logbook
- **Comprehensive Logging**: Record executed observation sessions with detailed parameters:
  - **Targets**: Name, object type, catalog IDs (Messier, NGC, IC, IC/NGC).
  - **Instruments**: Telescope/binocular name, type, aperture (mm), focal length (mm).
  - **Weather Conditions**: Sky conditions, temperature (°C), humidity (%), seeing, and transparency.
  - **Session Details**: Start/end timestamps, location parameters, and free-form observer notes.
- **Cloud Synchronization & Auth**:
  - Real-time synchronization powered by **Cloud Firestore** with offline caching.
  - Multi-provider **Firebase Authentication** (Email/Password with auto-registration and Anonymous guest access).
  - Local database fallback when offline or unauthenticated.

### ♿ Accessibility First (WCAG 2.2 Level AA)
- **Screen Reader Support**: Comprehensive `Semantics` coverage across list items, action buttons, form inputs, and modal pickers.
- **High-Visibility Focus**: Keyboard focus indicators and keyboard-navigable UI shell for desktop and hardware keyboard users.
- **Night Vision Red Theme**: Dedicated high-contrast Red Night Mode alongside standard Material 3 Dark and Light themes to preserve dark adaptation during field observation.
- **Touch & Click Targets**: All interactive elements conform to minimum 48×48 dp sizing guidelines.

### 📱 Cross-Platform UI
- **Adaptive Shell**: Material 3 Navigation Rail for Linux/desktop and NavigationBar for mobile displays.
- **Multi-Platform Targets**: Built and verified for Android, Linux Desktop, and Web.

---

## 🏗️ Architecture & Tech Stack

```text
lib/
├── app/                  # Application shell, router, themes (including Red Night Mode)
├── core/                 # Shared utilities, database helpers, Firebase & Auth providers
├── features/
│   ├── auth/             # Sign-in, user profile, anonymous auth logic
│   ├── planner/          # Observation planner, SQLite database, CSV handler, target models
│   ├── logbook/          # Observation logbook, Firestore repository, domain models, form views
│   └── settings/         # Instructions/How-To guides, theme toggles, and About page
└── main.dart             # Application entry point & initialization
```

- **Framework**: Flutter 3.x / Dart 3.x (Material 3)
- **State Management**: Riverpod (`flutter_riverpod`)
- **Navigation**: `go_router` with responsive `AppShell`
- **Database**: `sqflite` & `sqflite_common_ffi` (Local SQLite), `cloud_firestore` (Cloud)
- **Authentication**: `firebase_auth` & `firebase_core`
- **File I/O**: `csv` & `file_picker`
- **Date/Time Handling**: `intl`

---

## 🚀 Getting Started

### Prerequisites

- **Flutter SDK**: `^3.13.0` or higher
- **Android SDK**: For Android builds
- **Linux Build Dependencies**: `clang`, `cmake`, `ninja-build`, `pkg-config`, `libgtk-3-dev`

### Installation

1. Clone the repository and navigate to the `Astrolog` directory:
   ```bash
   cd Astrolog
   ```

2. Fetch Flutter dependencies:
   ```bash
   flutter pub get
   ```

3. Run the application on your desired target platform:
   - **Linux Desktop**:
     ```bash
     flutter run -d linux
     ```
   - **Android**:
     ```bash
     flutter run -d <android-device-id>
     ```
   - **Web**:
     ```bash
     flutter run -d chrome
     ```

---

## 📄 License & Credits

- **Developer**: Paul M. Summitt
- **Copyright**: © 2026 Paul M. Summitt
- **License**: MIT License
