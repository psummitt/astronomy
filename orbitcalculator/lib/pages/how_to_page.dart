import 'package:flutter/material.dart';

class HowToPage extends StatelessWidget {
  const HowToPage({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('How To Use')),
      body: const SingleChildScrollView(
        padding: EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'Getting Started',
              style: TextStyle(fontSize: 22, fontWeight: FontWeight.bold),
            ),
            SizedBox(height: 12),
            Text(
              '1. Enter the orbital elements for the object (comet, asteroid, etc.). These include Inclination, Argument of Perihelion, Longitude of Ascending Node, Reference Date (Julian Date), Eccentricity, and either Perihelion Distance (for parabolic/nearly parabolic) or Semi-major Axis (for elliptical orbits).',
            ),
            SizedBox(height: 8),
            Text(
              '2. Enter the Solar Coordinates for the dates you want to calculate the ephemeris. You will need the Date string, the Julian Date, and the X, Y, Z coordinates of the Sun for that epoch.',
            ),
            SizedBox(height: 8),
            Text(
              '3. Where to find data: Solar coordinates (Epoch 1950) can be found in the Astronomical Almanac, Section \'C\'.',
            ),
            SizedBox(height: 8),
            Text(
              '4. Toggle between Single and Double precision as needed. Single precision matches the original 1982 BASIC logic.',
            ),
            SizedBox(height: 16),
            Text(
              'Glossary:',
              style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
            ),
            SizedBox(height: 8),
            Text('• RA: Right Ascension (Hours and Minutes)'),
            Text('• Decl: Declination (Degrees and Minutes)'),
            Text('• AU: Astronomical Units (Distance Earth-Sun is ~1 AU)'),
          ],
        ),
      ),
    );
  }
}
