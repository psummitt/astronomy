import 'package:flutter/material.dart';

class InstructionsPage extends StatelessWidget {
  const InstructionsPage({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('AstroLog Instructions'),
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16.0),
        child: Semantics(
          label: 'Instructions for using AstroLog Planner and Logbook',
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                'How to Use AstroLog',
                style: Theme.of(context).textTheme.headlineMedium?.copyWith(
                      fontWeight: FontWeight.bold,
                    ),
              ),
              const SizedBox(height: 16),
              const _InstructionSection(
                title: '1. Target Planning (Planner)',
                icon: Icons.calendar_today,
                description:
                    'Use the Planner tab to schedule upcoming observation targets. Enter object names, scheduled date/time, Right Ascension (RA), Declination (Dec), and notes. Data is stored in a local SQLite database for offline field access.',
              ),
              const _InstructionSection(
                title: '2. CSV Import & Export',
                icon: Icons.swap_vert,
                description:
                    'Open the CSV options menu in the Planner header to export your planned target list to a .csv file or import target lists created in spreadsheet software.',
              ),
              const _InstructionSection(
                title: '3. Observation Logging (Logbook)',
                icon: Icons.menu_book,
                description:
                    'Use the Logbook tab to record executed astronomical observations. Capture target details, telescope/binocular specs, weather conditions, seeing/transparency, and observer notes. Logs synchronize in real-time via Cloud Firestore when connected.',
              ),
              const _InstructionSection(
                title: '4. Red Night Vision Mode',
                icon: Icons.remove_red_eye,
                description:
                    'In Settings, activate Red Night Vision Mode while observing under dark skies. This high-contrast red-on-black theme prevents loss of dark adaptation (scotopic vision).',
              ),
              const _InstructionSection(
                title: '5. Accessibility & Keyboard Control',
                icon: Icons.accessibility,
                description:
                    'AstroLog supports Linux desktop hardware keyboards and screen readers (TalkBack / VoiceOver). Use TAB to navigate between input fields with high-visibility focus indicators.',
              ),
            ],
          ),
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
