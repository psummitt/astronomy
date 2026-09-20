
import 'package:flutter/material.dart';

class HelpScreen extends StatelessWidget {
  const HelpScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('How to Use PStar'),
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16.0),
        child: Semantics(
          label: 'Help Content',
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                'About PStar',
                style: Theme.of(context).textTheme.headlineSmall,
              ),
              const SizedBox(height: 8),
              const Text(
                'This program provides the times of elongations and transits of Polaris for any date with sufficient accuracy (within 10 minutes) for setting or checking the alignment of the equatorial mount of a telescope.',
              ),
              const SizedBox(height: 16),
              Text(
                'Instructions',
                style: Theme.of(context).textTheme.headlineSmall,
              ),
              const SizedBox(height: 8),
              const Text('1. Enter the Year, Month, and Day you are interested in.'),
              const Text('2. Enter your Longitude in degrees (e.g., 122.3 for Seattle, -74.0 for New York). Note: East is positive, West is usually negative, but follow the original formula convention if it differs.'),
              const Text('3. Select the type of event you want to calculate:'),
              Padding(
                padding: const EdgeInsets.only(left: 16.0, top: 4.0),
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: const [
                    Text('• West Elongation (Next/Previous)'),
                    Text('• East Elongation (Next/Previous)'),
                    Text('• Lower Transit (Next)'),
                    Text('• Upper Transit (Next)'),
                  ],
                ),
              ),
              const SizedBox(height: 8),
              const Text('4. The result will show the time in decimal hours and in HR:MI format.'),
              const SizedBox(height: 16),
              Text(
                'Credits',
                style: Theme.of(context).textTheme.headlineSmall,
              ),
              const SizedBox(height: 8),
              const Text('Original BASIC program by Eric Burgess F.R.A.S.'),
              const Text('All rights reserved by S & T Software Service (1980).'),
              const Text('Converted to Flutter 7/6/2026'),
            ],
          ),
        ),
      ),
    );
  }
}
