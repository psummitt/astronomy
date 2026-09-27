import 'package:flutter/material.dart';

class HelpScreen extends StatelessWidget {
  const HelpScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Help Guide')),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(24),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            const Text('How to Use Sunfinder', style: TextStyle(fontSize: 24, fontWeight: FontWeight.bold)),
            const SizedBox(height: 16),
            _buildStep('1. Configure Settings', 'Go to the Settings page and enter your Latitude, Longitude, Standard Meridian, and Magnetic Variation. Ensure you use negative values for South and East as per the original program conventions.'),
            _buildStep('2. Choose a Tool', 'Select "Where\'s the Sun" for positional data (Altitude and Azimuth) at specific hours, or "Sunrise & Sunset" for daily event times.'),
            _buildStep('3. Enter Dates', 'Provide the month and day for both start and end ranges. You can also specify the increment (step) in hours or days to filter results.'),
            _buildStep('4. Interpret Results', 'Altitude is the angle above the horizon (0-90°). Azimuth is the bearing from North (0-360°). Results are calculated based on your local standard time.'),
            const SizedBox(height: 24),
            const Text('Accessibility', style: TextStyle(fontSize: 20, fontWeight: FontWeight.bold)),
            const Divider(),
            const Text('This app is fully compatible with screen readers and keyboard navigation. Use the Tab key on Desktop/Web to move between fields.'),
            const SizedBox(height: 32),
            Center(
              child: ElevatedButton(
                onPressed: () => Navigator.pop(context),
                child: const Text('Got it!'),
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildStep(String title, String description) {
    return Padding(
      padding: const EdgeInsets.only(bottom: 20),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(title, style: const TextStyle(fontSize: 18, fontWeight: FontWeight.bold, color: Colors.amber)),
          const SizedBox(height: 4),
          Text(description, style: const TextStyle(fontSize: 16)),
        ],
      ),
    );
  }
}
