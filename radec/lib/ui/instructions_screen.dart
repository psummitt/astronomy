import 'package:flutter/material.dart';

class InstructionsScreen extends StatelessWidget {
  const InstructionsScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      padding: const EdgeInsets.all(24.0),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            'How to Use the Radec App',
            style: Theme.of(context).textTheme.headlineMedium,
          ),
          const SizedBox(height: 16),
          const Text(
            'This application provides precise positions of all planets in our solar system for a given date. You can choose between two methods of calculation:',
          ),
          const SizedBox(height: 16),
          _buildInstructionPoint(
            context,
            '1. Local (BASIC) Calculation',
            'This mode uses the original mathematical algorithms ported from the 1970s BASIC program. It works offline and provides good approximations for most historical and near-future dates.',
          ),
          const SizedBox(height: 12),
          _buildInstructionPoint(
            context,
            '2. Internet (NASA JPL) Mode',
            'This mode fetches high-precision data from the NASA JPL Horizons API and the Visible Planets API. It requires an active internet connection. Note: On web browsers, data access may be limited due to security (CORS) restrictions on scientific servers.',
          ),
          const SizedBox(height: 24),
          Text(
            'Navigating the Results',
            style: Theme.of(context).textTheme.titleLarge,
          ),
          const SizedBox(height: 8),
          const Text(
            '• RA (Right Ascension): Measured in hours, minutes, and seconds, indicating the planet\'s position along the celestial equator.\n'
            '• Dec (Declination): Measured in degrees, indicating how far north or south the planet is from the celestial equator.\n'
            '• Sun (AU): Heliocentric distance — how far the planet is from the Sun in Astronomical Units.\n'
            '• Earth (AU): Geocentric distance — how far the planet is from Earth in Astronomical Units.',
          ),
          const SizedBox(height: 24),
          const Card(
            child: Padding(
              padding: EdgeInsets.all(16.0),
              child: Text(
                'Accessibility Tip: This app is fully compatible with screen readers. Use the drawer menu to navigate between the calculator, instructions, and history pages.',
                style: TextStyle(fontStyle: FontStyle.italic),
              ),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildInstructionPoint(BuildContext context, String title, String body) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(title, style: const TextStyle(fontWeight: FontWeight.bold)),
        const SizedBox(height: 4),
        Text(body),
      ],
    );
  }
}
