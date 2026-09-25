# PRISE

PRISE (planetary rise and set) is a Flutter application for estimating when the Sun and planets cross the horizon. It runs on Android, Linux, and the web without a network connection.

## Run

From this directory:

```bash
flutter pub get
flutter run -d chrome
flutter run -d linux
flutter run -d <android-device>
```

## Build

```bash
flutter build web
flutter build linux
flutter build apk --debug
```

## Using the calculator

Choose a body and date, enter latitude and longitude in decimal degrees, and enter the civil time offset from UTC. Latitude is positive north and longitude is positive east. Results show local and UTC rise and set times.

The calculation is a compact, low-precision orbital model with a standard horizon altitude. It does not model refraction, terrain, or atmospheric conditions, so use a professional ephemeris for navigation or scientific work.

## Accessibility

The app provides labeled controls, keyboard navigation, visible focus treatment, high-contrast colors, large touch targets, readable text, semantic result summaries, and live announcements when a result changes. The Instructions and About pages are available from the bottom navigation bar.

## Source note

The repository did not contain a `PRISE.BAS` listing when this app was created. The About page records this assumption and identifies the implementation as a self-contained interpretation of the rise-and-set program described in *Celestial BASIC* by Eric Burgess.
