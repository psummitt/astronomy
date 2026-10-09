# AstroDiary

**AstroDiary** is a multi-platform Flutter application designed for amateur and professional astronomers. It combines an interactive observation **Calendar**, an **Observation Planner** with modern automated coordinate calculations, and an **Observation Logbook** for recording field sessions under the night sky.

Supported Platforms: **Android**, **Linux Desktop**, **Web**, and **Windows Desktop**.

---

## 🌟 Key Features

### 📅 1. Calendar-Like Presentation
- **Interactive Calendar View**: Browse scheduled target plans and completed observation logs on a monthly calendar grid.
- **Visual Event Markers**:
  - 🔵 **Blue Dot**: Scheduled target observation (`PlannerObservation`).
  - 🟢 **Green Dot**: Recorded observation log (`ObservationLog`).
- **Day Detail Panel**: Select any date to view scheduled targets and logged sessions, or quickly add new entries pre-filled for that day.

### 🔭 2. Automated Astronomical RA/DEC Calculations
When creating a planned target, enter the target name to automatically compute Right Ascension (RA) and Declination (Dec):
- **Moon**: Automatically calculates high-accuracy lunar positions using modern Meeus algorithms (ported from `radem`).
- **Planets** (*Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto*): Computes planetary position coordinates using Keplerian orbital models (ported from `radem`/`radec`).
- **Stars** (*e.g., Sirius, Betelgeuse, Vega, Polaris, M31*): Queries the CDS Strasbourg Sesame astronomical database to resolve J2000 coordinates (ported from `starsearch`).

### 📝 3. Observation Target Planner
- Schedule upcoming observing targets with date, time, RA, Dec, and observer notes.
- Offline storage powered by local SQLite database.
- **CSV Import & Export**: Easily import target lists or export plans to `.csv` format.

### 📖 4. Observation Logbook
- Record completed telescopic and binocular observation sessions.
- Track instrument specifications (aperture, focal length), weather conditions, seeing/transparency ratings, and field notes.
- Real-time cloud sync and backup via Firebase Firestore when signed in.

### 🔴 5. Red Night Vision Mode
- Built-in field modes including Light, Dark Space, and **Red Night Vision Mode**.
- High-contrast pure red-on-black interface designed to preserve dark adaptation (scotopic vision) at field observing sites.

---

## 📁 Directory & Code Structure

```
astrodiary/
├── android/                   # Android Gradle build configuration
├── linux/                     # Linux GTK desktop build configuration
├── web/                       # Web CanvasKit/HTML configuration
├── windows/                   # Windows CMake build configuration
├── lib/
│   ├── app/
│   │   ├── app.dart           # App entry MaterialApp configuration
│   │   ├── app_shell.dart     # Responsive navigation rail / bottom bar
│   │   ├── router.dart        # GoRouter navigation configuration
│   │   └── theme.dart         # Light, Dark, & Red Night Vision themes
│   ├── core/
│   │   ├── firebase_options.dart
│   │   └── providers/         # Theme & Auth Riverpod state providers
│   ├── features/
│   │   ├── calendar/          # Interactive Calendar view
│   │   ├── planner/           # Observation Planner & CSV handler
│   │   ├── logbook/           # Observation Logbook & Firestore repository
│   │   ├── auth/              # Sign in & Guest access
│   │   └── settings/          # App settings, Red Night mode, instructions
│   ├── services/
│   │   ├── moon_calculator.dart   # RADEM Modern lunar algorithm
│   │   ├── planet_calculator.dart # RADEM/RADEC planetary algorithm
│   │   └── star_service.dart      # CDS Strasbourg star lookup
│   └── main.dart
├── astrodiary implementation plan.md
├── history.md                 # Session prompt & response history
└── pubspec.yaml
```

---

## 🚀 Getting Started

### Prerequisites
- [Flutter SDK](https://flutter.dev/docs/get-started/install) (version 3.0.0 or higher)
- Dart SDK 3.x

### Installation & Setup

1. **Clone the repository** (if not already local):
   ```bash
   git clone https://github.com/paulmsummitt/astronomy.git
   cd astronomy/astrodiary
   ```

2. **Fetch dependencies**:
   ```bash
   flutter pub get
   ```

3. **Run the Application**:

   - **Linux Desktop**:
     ```bash
     flutter run -d linux
     ```

   - **Web**:
     ```bash
     flutter run -d chrome
     ```

   - **Windows Desktop**:
     ```bash
     flutter run -d windows
     ```

   - **Android**:
     ```bash
     flutter run -d android
     ```

---

## 📄 Documentation

- [Implementation Plan](file:///home/paulmsummitt/github/astronomy/astrodiary/astrodiary%20implementation%20plan.md)
- [Session History Log](file:///home/paulmsummitt/github/astronomy/astrodiary/history.md)

---

## 📜 License

© 2026 Paul M. Summitt. Licensed under the MIT License.
