# StarSearch

StarSearch is a cross-platform Flutter application (Android, Linux, Web, Windows) that allows users to search for stars by name and retrieve their celestial and local coordinate data.

## Features

*   **Star Search**: Search for stars by name (e.g., Sirius, Betelgeuse).
*   **Celestial Data**: Get Brightness (Magnitude), Right Ascension (RA), and Declination (Dec).
*   **Local Position**: Calculate real-time Altitude and Bearing (Azimuth) based on the user's current location.
*   **Accessibility**: Built with accessibility in mind, supporting screen readers and dynamic text.
*   **How to Use**: Integrated guide for new users.

## Requirements

*   **Internet Access**: Required to fetch star data from the CDS Sesame API.
*   **Location Permissions**: Required to calculate Altitude and Bearing.

## Tech Stack

*   **Flutter**: Framework for multi-platform development.
*   **http**: For API communication.
*   **astronomia**: For astronomical calculations.
*   **geolocator**: For retrieving the observer's location.
*   **xml**: For parsing API responses.

## Getting Started

1.  Run `flutter pub get` to install dependencies.
2.  Run the app using `flutter run` on your desired platform.
