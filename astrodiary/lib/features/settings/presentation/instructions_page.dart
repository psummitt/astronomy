import 'package:flutter/material.dart';

class InstructionsPage extends StatelessWidget {
  const InstructionsPage({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('AstroDiary Instructions'),
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'How to Use AstroDiary',
              style: Theme.of(context).textTheme.headlineMedium?.copyWith(
                    fontWeight: FontWeight.bold,
                  ),
            ),
            const SizedBox(height: 16),
            const _InstructionSection(
              title: '1. Calendar Presentation',
              icon: Icons.calendar_month,
              description:
                  'Browse scheduled target plans (blue dots) and recorded observation logs (green dots) in the Calendar tab. Tap any date to view planned and completed events.',
            ),
            const _InstructionSection(
              title: '2. Target Planning with Modern Calculations',
              icon: Icons.auto_awesome,
              description:
                  'When adding a target plan, enter "Moon", a planet name (e.g. Jupiter, Mars), or a star name (e.g. Sirius). RA and Dec are automatically computed using modern RADEM lunar/planetary algorithms or CDS Strasbourg Sesame star catalog data.',
            ),
            const _InstructionSection(
              title: '3. Observation Logging',
              icon: Icons.menu_book,
              description:
                  'Use the Logbook tab to record detailed logs of your telescopic observation sessions, including weather conditions, eyepieces, seeing/transparency, and observer notes.',
            ),
            const _InstructionSection(
              title: '4. Red Night Vision Mode',
              icon: Icons.remove_red_eye,
              description:
                  'In Settings, switch to Red Night Vision Mode while observing outdoors under dark skies to preserve your scotopic vision.',
            ),
          ],
        ),
      ),
    );
  }
}

class _InstructionSection extends StatelessWidget {
  final String title;
  final IconData icon;
  final String description;

  const _InstructionSection({
    required this.title,
    required this.icon,
    required this.description,
  });

  @override
  Widget build(BuildContext context) {
    return Padding(
      padding: const EdgeInsets.only(bottom: 20.0),
      child: Card(
        child: Padding(
          padding: const EdgeInsets.all(16.0),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Row(
                children: [
                  Icon(icon, color: Theme.of(context).colorScheme.primary),
                  const SizedBox(width: 8),
                  Expanded(
                    child: Text(
                      title,
                      style: Theme.of(context).textTheme.titleMedium?.copyWith(
                            fontWeight: FontWeight.bold,
                          ),
                    ),
                  ),
                ],
              ),
              const SizedBox(height: 8),
              Text(
                description,
                style: Theme.of(context).textTheme.bodyMedium,
              ),
            ],
          ),
        ),
      ),
    );
  }
}
