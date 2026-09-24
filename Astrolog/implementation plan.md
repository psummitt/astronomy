# AstroLog Implementation Plan

## 1. Project Overview

**Goal**: Rebuild and expand **AstroLog** into a unified, production-ready Flutter application targeting **Android**, **Linux Desktop**, and **Web** (preserving the existing web build in `Astrolog/build/web` without disruption). AstroLog combines the capabilities of `astroplanner` (observation target planning, celestial coordinates, SQLite/SharedPreferences storage, CSV import/export) and `logbook` (observation logbook tracking targets, instruments, weather, notes, Cloud Firestore real-time sync, and Firebase Authentication).

---

## 2. Technical Stack & Dependencies

| Area | Package / Technology | Purpose |
|---|---|---|
| **UI Framework** | Flutter (Material 3) | Declarative cross-platform UI |
| **State Management** | `flutter_riverpod` | Reactive state management & provider dependency injection |
| **Routing** | `go_router` | ShellRoute navigation, deep linking, tab state management |
| **Local Persistence** | `sqflite` + `sqflite_common_ffi` + `shared_preferences` | Local SQLite database on Android/Linux, SharedPreferences on Web |
| **Cloud Synchronization** | `cloud_firestore` | Real-time observation logbook sync with offline caching |
| **Authentication** | `firebase_auth` + `firebase_core` | Email/Password and Anonymous sign-in |
| **Data Exchange** | `csv` + `file_picker` | Export and import observation plans as CSV files |
| **Utilities & Format** | `intl`, `path`, `path_provider` | Formatting, date/time pickers, and local directory resolution |

---

## 3. Architecture & Unified Application Structure

The application follows a clean feature-first architecture:

```text
Astrolog/
├── android/              # Native Android configuration
├── linux/                # Native Linux desktop runner configuration
├── web/                  # Native Web configuration
├── build/web/            # Existing production web build (preserved intact)
├── lib/
│   ├── app/              # App routing, themes (Dark, Light, Red Night Mode), AppShell
│   ├── core/             # Shared database helpers, Firebase & Auth providers, accessibility helpers
│   ├── features/
│   │   ├── planner/      # Target planning models, SQLite helper, CSV handler, provider, UI screens
│   │   ├── logbook/      # Logbook models, Firestore repository, list/detail/form screens
│   │   ├── auth/         # Login & guest profile UI
│   │   └── settings/     # How-to instructions, theme selector, About screen
│   └── main.dart         # Entry point, Firebase & SQLite FFI initialization
├── pubspec.yaml          # Unified package definition
└── README.md             # Complete user and developer documentation
```

---

## 4. Key Implementation Features

### A. Unified Application Shell (`AppShell`)
- **Responsive Layout**: Automatically switches between a desktop `NavigationRail` (Linux/Web) and mobile `NavigationBar` (Android).
- **Navigation Destinations**:
  1. **Planner**: Upcoming target planning list, RA/Dec coordinates, add plan, CSV export/import options.
  2. **Logbook**: Logged observation history, detailed session cards, Cloud Firestore sync, CRUD operations.
  3. **Instructions**: Combined usage guide for both planning and observation logging.
  4. **Account & Settings**: Firebase Auth login, theme toggle (Dark, Light, High-Contrast Red Night Mode), and About information.

### B. Planner Integration
- **Model**: `PlannerObservation` (Target Name, Date & Time, RA, Dec, Notes).
- **Local SQLite Engine**: Initialized with `sqflite_common_ffi` on Linux and `sqflite` on Android; fallback memory/preferences strategy on Web.
- **CSV Data Exchange**:
  - Export plan to `.csv` file via `CsvHandler.exportToCsv()`.
  - Import plan from `.csv` file with validation via `CsvHandler.importFromCsv()`.

### C. Logbook Integration
- **Model**: `ObservationLog` (Target details, Weather, Instrument specs, Location, Timestamps, Notes).
- **Cloud Firestore Repository**: Real-time stream of user observations with `userId` security isolation.
- **Local Fallback**: Local caching when offline or running without active Firebase connection.

### D. Accessibility Standards (WCAG 2.2 Level AA)
- **Screen Reader Optimization**: Explicit `Semantics` tags with descriptive labels, buttons, headers, and interaction hints.
- **Red Night Vision Theme**: Dedicated red-on-black astronomical color theme to preserve dark adaptation in the field.
- **High-Visibility Focus**: High-contrast `focusColor` outlines for Linux desktop keyboard navigation.
- **Minimum Touch Targets**: All interactive elements (buttons, list items, icon actions) feature a minimum size of 48×48 dp.

---

## 5. Execution Phases

### Phase 1: Project Setup & Package Configuration
- Configure `pubspec.yaml` with all required dependencies (`flutter_riverpod`, `go_router`, `firebase_core`, `firebase_auth`, `cloud_firestore`, `sqflite`, `sqflite_common_ffi`, `csv`, `file_picker`, `shared_preferences`, `intl`).
- Initialize native Android and Linux platform code.

### Phase 2: Core Infrastructure & Navigation Shell
- Create `AppTheme` with Material 3 Light, Dark, and Red Night Vision palettes.
- Create `AppShell` with responsive `NavigationRail` and `NavigationBar`.
- Set up `go_router` for route management across all features.

### Phase 3: Planner Feature Implementation
- Port and adapt SQLite database helper (`DatabaseHelper`), CSV handler (`CsvHandler`), and Riverpod state management.
- Implement Planner UI screens (`PlannerListPage`, `AddPlanPage`, `PlannerHowToPage`) with full accessibility semantics.

### Phase 4: Logbook Feature Implementation
- Port domain models (`Observation`, `Weather`, `Target`, `Instrument`, `Location`).
- Configure Firestore repository and Firebase authentication providers (`auth_providers`).
- Implement Logbook UI screens (`ObservationListPage`, `ObservationDetailPage`, `ObservationFormPage`, `LoginPage`) with accessibility markup.

### Phase 5: Settings & Guidance
- Build `SettingsPage` with theme switcher (Light, Dark, Red Night Mode).
- Build `InstructionsPage` providing clear operational instructions.
- Build `AboutPage` with version and license information.

### Phase 6: Verification & Quality Assurance
- Perform Flutter static code analysis (`flutter analyze`).
- Run unit/widget tests (`flutter test`).
- Verify Linux build (`flutter build linux`).
- Verify Android build readiness.
- Confirm `Astrolog/build/web` build integrity.

---

## 6. Definition of Done

- [x] Clear, comprehensive `README.md` and `implementation plan.md` in `Astrolog/`.
- [ ] Dependencies configured in `pubspec.yaml` and resolved cleanly.
- [ ] Responsive navigation shell operational across desktop and mobile layout breakpoints.
- [ ] Planner features working (SQLite persistence, CSV import/export).
- [ ] Logbook features working (Firestore sync, Firebase Auth, CRUD).
- [ ] Accessibility standards met (Semantics, Focus, Red Night Theme, 48dp touch targets).
- [ ] Android, Linux, and Web targets build without errors.
- [ ] Existing `Astrolog/build/web` build remains completely unharmed.
