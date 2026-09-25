# PRISE

PRISE (planetary rise and set) is a Flutter application for estimating when Mercury or Venus rises before the Sun or sets after it. It runs on Android, Linux, and the web without a network connection.

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

Choose Mercury or Venus and a date, then enter latitude and longitude in decimal degrees. Latitude is positive north and longitude is positive east. Results estimate the planet's rise or set offset from the Sun.

The calculation is a compact, low-precision orbital model with a standard horizon altitude. It follows PRISE.BAS's purpose but is not a line-for-line port. It does not model refraction, terrain, or atmospheric conditions, so use a professional ephemeris for navigation or scientific work.

## Accessibility

The app provides labeled controls, keyboard navigation, visible focus treatment, high-contrast colors, large touch targets, readable text, semantic result summaries, and live announcements when a result changes. Changing the selected body or date recalculates the estimate. The Instructions and About pages are available from the bottom navigation bar.

## Source note

The supplied `PRISE.BAS` listing estimates when Mercury and Venus rise before the Sun as morning stars or set after it as evening stars. This app follows that behavior using a modern low-precision orbital model rather than reproducing the original BASIC calculations line for line.
