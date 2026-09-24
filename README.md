# Astronomy Software & Applications Suite

A comprehensive repository of astronomical software applications, observational planning tools, coordinate transformation algorithms, time/calendar conversion libraries, and celestial ephemeris utilities.

---

## 🌌 Repository Overview

This workspace brings together cross-platform desktop/mobile/web applications alongside native C and Fortran calculation libraries for amateur and professional astronomers.

```text
astronomy/
├── Astrolog/                 # Unified Flutter App (Android, Linux Desktop, Web)
├── astroplanner/             # Standalone observation planner app
├── logbook/                  # Standalone observation logbook app
├── Sunfinder/                # Solar positioning & solar tracking calculators
├── eclipse/                  # Eclipse prediction & calculation tools
├── radec/ & radem/           # Right Ascension & Declination coordinate converters
├── orbitcalculator/          # Orbital mechanics & trajectory calculators
├── starsearch/               # Star catalog search utilities
├── mercury/                  # Inner planet ephemeris routines
├── Julian/                   # Julian Date & Sidereal Time routines
├── Epoch/                    # Astronomical epoch precession routines (J2000/B1950)
├── Calendar/ & CDate/        # Calendar date transformation algorithms
├── Easter/                   # Computus calendar algorithms
├── PStar/                    # Polar alignment & polar star tools
└── FortranAstronomyProgram/  # Fortran 77/90 time routines with C bindings
```

---

## 📱 Primary Applications

### [AstroLog](Astrolog/)
The primary cross-platform astronomical application built with **Flutter** targeting **Android**, **Linux Desktop**, and **Web**:
- **Target Planner**: Scheduled target observation list, celestial coordinates (RA/Dec), local SQLite storage (`sqflite_common_ffi`), and CSV import/export.
- **Observation Logbook**: Record completed observation sessions (instruments, weather, sky conditions, notes) with Cloud Firestore sync and Firebase Authentication.
- **Field Accessibility (WCAG 2.2 AA)**: Screen reader semantics, desktop focus outlines, and a high-contrast **Red Night Vision Mode** to preserve scotopic vision under dark skies.

### [AstroPlanner](astroplanner/)
Standalone Flutter application focused on target observation planning, celestial coordinate tracking, and CSV file exchange.

### [Logbook](logbook/)
Standalone Flutter application focused on observation session logging and Cloud Firestore synchronization.

---

## 🔭 Solar, Eclipse & Planetary Modules

- **[Sunfinder](Sunfinder/)**: Solar position calculation and solar array tracking algorithms.
- **[eclipse](eclipse/)**: Solar and lunar eclipse prediction and geometry routines.
- **[mercury](mercury/)**: Ephemeris and orbital positioning routines for Mercury and inner planets.

---

## 🌌 Coordinate Systems & Orbital Mechanics

- **[radec](radec/) & [radem](radem/)**: Right Ascension (RA) and Declination (Dec) coordinate transformations, epoch precession, and nutation adjustments.
- **[orbitcalculator](orbitcalculator/)**: Orbital mechanics calculations, Keplerian elements processing, and celestial trajectories.
- **[starsearch](starsearch/)**: Star catalog querying and astronomical object lookup.

---

## ⏱️ Time, Epoch & Calendar Libraries

- **[Julian](Julian/)**: Conversions between Gregorian Date, Julian Date (JD), Modified Julian Date (MJD), and Sidereal Time.
- **[Epoch](Epoch/)**: Epoch coordinate precession routines between standard equinoxes (e.g., J2000.0 and B1950.0).
- **[Calendar](Calendar/) & [CDate](CDate/)**: Date transformation and calendar systems conversion algorithms.
- **[Easter](Easter/)**: Ecclesiastical Computus calendar calculations.
- **[PStar](PStar/)**: Polar star calculations and equatorial mount polar alignment tools.

---

## 💻 Native & Legacy Libraries

- **[FortranAstronomyProgram](FortranAstronomyProgram/)**: Fortran 77/90 astronomical time library (`Time.f`, `main.f90`) with C/C++ interoperability headers in `bind/`.
- **`pendant.c`**: Native C utility script for astronomical calculation support.

---

## 🛠️ Build & Requirements

- **Flutter Applications (`Astrolog`, `astroplanner`, `logbook`)**: Require Flutter SDK `^3.13.0` or higher, Android SDK for mobile, and `clang`/`cmake`/`ninja-build`/`libgtk-3-dev` for Linux desktop builds.
- **C/Fortran Projects (`FortranAstronomyProgram`, etc.)**: Require `gcc`, `gfortran`, `make`, or Code::Blocks IDE.

---

## 📄 License & Credits

- **Developer**: Paul M. Summitt
- **Copyright**: © 2026 Paul M. Summitt
