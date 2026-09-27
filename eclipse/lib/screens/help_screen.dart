import 'package:flutter/material.dart';

class HelpScreen extends StatelessWidget {
  const HelpScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Help & Instructions')),
      body: ListView(
        padding: const EdgeInsets.all(24),
        children: [
          _section(context, 'About the App', 
            'This application calculates the date and magnitude of Lunar Umbral Eclipses. '
            'It is a modern recreation of the original Burgess logic combined with '
            'modern high-precision algorithms.'),
          
          _section(context, 'How to Use', 
            '1. Enter the starting year in the input field.\n'
            '2. Select the calculation engine (Modern or Legacy).\n'
            '3. Tap "Calculate Eclipses" to view the results.'),

          _section(context, 'Calculation Engines', 
            '• Modern: High-precision algorithm using modern astronomical data (Meeus algorithm).\n'
            '• Legacy: A faithful port of the original 1980 BASIC logic.'),

          _section(context, 'Visibility Filtering', 
            'You can filter eclipses to show only those visible from your specific geographic location. '
            'A lunar eclipse is visible if the Moon is above your horizon at the time of greatest eclipse. '
            'Enter your Latitude and Longitude on the home screen to enable this feature.'),
          
          _section(context, 'Accessibility', 
            'This app is designed to be fully accessible:\n'
            '• Screen Readers: All buttons and inputs have semantic labels.\n'
            '• High Contrast: Themes adapt to light and dark modes.\n'
            '• Font Scaling: UI elements adjust to system font sizes.'),

          _section(context, 'Glossary', 
            '• Total Lunar Eclipse: The entire Moon passes through the Earth\'s darkest shadow (Umbra).\n'
            '• Partial Lunar Eclipse: Only a portion of the Moon enters the Umbra.\n'
            '• Penumbral Lunar Eclipse: The Moon passes through the Earth\'s outer, lighter shadow (Penumbral). This is a subtle dimming and can be hard to see.\n'
            '• Magnitude: The fraction of the Moon\'s diameter covered by the shadow.'),
          
          const SizedBox(height: 40),
          const Divider(),
          const Text(
            'Original Code © S & T Software Services\nModern Flutter Implementation 2024',
            textAlign: TextAlign.center,
            style: TextStyle(fontStyle: FontStyle.italic, color: Colors.grey),
          ),
        ],
      ),
    );
  }

  Widget _section(BuildContext context, String title, String content) {
    return Padding(
      padding: const EdgeInsets.only(bottom: 24),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(title, style: Theme.of(context).textTheme.titleLarge?.copyWith(color: Colors.indigo, fontWeight: FontWeight.bold)),
          const SizedBox(height: 8),
          Text(content, style: Theme.of(context).textTheme.bodyLarge),
        ],
      ),
    );
  }
}
