import 'package:flutter/material.dart';

class AboutPage extends StatelessWidget {
  const AboutPage({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('About AstroLog'),
      ),
      body: Center(
        child: SingleChildScrollView(
          padding: const EdgeInsets.all(24.0),
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              Icon(
                Icons.nights_stay,
                size: 80,
                color: Theme.of(context).colorScheme.primary,
              ),
              const SizedBox(height: 16),
              Text(
                'AstroLog',
                style: Theme.of(context).textTheme.headlineLarge?.copyWith(
                      fontWeight: FontWeight.bold,
                    ),
              ),
              const SizedBox(height: 4),
              Text(
                'Version 1.0.0+1',
                style: Theme.of(context).textTheme.bodyLarge?.copyWith(
                      color: Theme.of(context).colorScheme.secondary,
                    ),
              ),
              const SizedBox(height: 16),
              const Padding(
                padding: EdgeInsets.symmetric(horizontal: 16.0),
                child: Text(
                  'A unified astronomical target planner and observation logbook application built for Android, Linux Desktop, and Web.',
                  textAlign: TextAlign.center,
                ),
              ),
              const SizedBox(height: 32),
              const Divider(),
              const SizedBox(height: 16),
              Text(
                'Developed by Paul M. Summitt',
                style: Theme.of(context).textTheme.titleMedium?.copyWith(
                      fontWeight: FontWeight.bold,
                    ),
              ),
              const SizedBox(height: 4),
              const Text('© 2026 Paul M. Summitt'),
              const SizedBox(height: 4),
              const Text('Licensed under the MIT License'),
              const SizedBox(height: 24),
              ElevatedButton.icon(
                onPressed: () => showLicensePage(
                  context: context,
                  applicationName: 'AstroLog',
                  applicationVersion: '1.0.0+1',
                ),
                icon: const Icon(Icons.description_outlined),
                label: const Text('View Software Licenses'),
              ),
            ],
          ),
        ),
      ),
    );
  }
}
