import 'package:flutter/material.dart';

class HowToUsePage extends StatelessWidget {
  const HowToUsePage({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('How to Use StarSearch'),
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Semantics(
              header: true,
              child: Text(
                'Searching for a Star',
                style: Theme.of(context).textTheme.headlineSmall,
              ),
            ),
            const SizedBox(height: 8),
            const Text(
              'Enter the name of a star (e.g., "Sirius", "Betelgeuse", "Vega") or any other celestial object (such as the Whirlpool Galaxy) in the search bar on the home screen and press the search icon or Enter key.',
            ),
            const SizedBox(height: 24),
            Semantics(
              header: true,
              child: Text(
                'Understanding the Data',
                style: Theme.of(context).textTheme.headlineSmall,
              ),
            ),
            const SizedBox(height: 8),
            const ListTile(
              leading: Icon(Icons.brightness_5),
              title: Text('Brightness (Magnitude)'),
              subtitle: Text('Lower values indicate brighter objects. Sirius is around -1.46.'),
            ),
            const ListTile(
              leading: Icon(Icons.explore),
              title: Text('Right Ascension (RA) & Declination (Dec)'),
              subtitle: Text('Celestial coordinates, similar to longitude and latitude on Earth.'),
            ),
            const ListTile(
              leading: Icon(Icons.height),
              title: Text('Altitude'),
              subtitle: Text('The angle of the star above the horizon (0° to 90°).'),
            ),
            const ListTile(
              leading: Icon(Icons.compass_calibration),
              title: Text('Bearing (Azimuth)'),
              subtitle: Text('The compass direction of the star (0° North, 90° East, etc.).'),
            ),
            const SizedBox(height: 24),
            Semantics(
              header: true,
              child: Text(
                'Accessibility',
                style: Theme.of(context).textTheme.headlineSmall,
              ),
            ),
            const SizedBox(height: 8),
            const Text(
              'This app is designed to be fully accessible. It supports screen readers (TalkBack/VoiceOver), dynamic text sizing, and high contrast themes. All interactive elements have clear semantic labels.',
            ),
          ],
        ),
      ),
    );
  }
}
