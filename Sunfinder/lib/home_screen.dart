import 'package:flutter/material.dart';
import 'settings_model.dart';
import 'main.dart';
import 'settings_screen.dart';
import 'sun_position_screen.dart';
import 'sunrise_sunset_screen.dart';
import 'help_screen.dart';
import 'history_screen.dart';

class HomeScreen extends StatelessWidget {
  final SunfinderSettings settings;
  const HomeScreen({super.key, required this.settings});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Sunfinder'),
        actions: [
          IconButton(
            icon: const Icon(Icons.settings),
            onPressed: () => Navigator.push(
              context,
              MaterialPageRoute(builder: (context) => SettingsScreen(settings: settings)),
            ),
            tooltip: 'Settings',
          ),
          IconButton(
            icon: const Icon(Icons.terminal),
            onPressed: () {
              final newSettings = settings;
              newSettings.isModernMode = false;
              SunfinderApp.of(context).updateSettings(newSettings);
            },
            tooltip: 'Switch to Classic Mode',
          ),
        ],
      ),
      body: Center(
        child: ConstrainedBox(
          constraints: const BoxConstraints(maxWidth: 600),
          child: ListView(
            padding: const EdgeInsets.all(24),
            children: [
              _buildHeader(context),
              const SizedBox(height: 32),
              _buildToolCard(
                context,
                title: "Where's the Sun",
                subtitle: "Calculate solar altitude and azimuth for specific times.",
                icon: Icons.wb_sunny,
                onTap: () => Navigator.push(
                  context,
                  MaterialPageRoute(builder: (context) => SunPositionScreen(settings: settings)),
                ),
              ),
              const SizedBox(height: 16),
              _buildToolCard(
                context,
                title: "Sunrise & Sunset",
                subtitle: "Find sunrise, sunset, and local noon details.",
                icon: Icons.wb_twilight,
                onTap: () => Navigator.push(
                  context,
                  MaterialPageRoute(builder: (context) => SunriseSunsetScreen(settings: settings)),
                ),
              ),
              const SizedBox(height: 32),
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: [
                  TextButton.icon(
                    onPressed: () => Navigator.push(
                      context,
                      MaterialPageRoute(builder: (context) => const HelpScreen()),
                    ),
                    icon: const Icon(Icons.help_outline),
                    label: const Text('Help Guide'),
                  ),
                  TextButton.icon(
                    onPressed: () => Navigator.push(
                      context,
                      MaterialPageRoute(builder: (context) => const HistoryScreen()),
                    ),
                    icon: const Icon(Icons.history_edu),
                    label: const Text('Program History'),
                  ),
                ],
              ),
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildHeader(BuildContext context) {
    return Column(
      children: [
        const Icon(Icons.sunny, size: 80, color: Colors.amber),
        const SizedBox(height: 16),
        Text(
          'Sunfinder',
          style: Theme.of(context).textTheme.headlineLarge?.copyWith(
            fontWeight: FontWeight.bold,
          ),
        ),
        Text(
          'Based on Harris Smith\'s 1983 TRS-80 Program',
          style: Theme.of(context).textTheme.bodyMedium,
          textAlign: TextAlign.center,
        ),
      ],
    );
  }

  Widget _buildToolCard(BuildContext context, {
    required String title,
    required String subtitle,
    required IconData icon,
    required VoidCallback onTap,
  }) {
    return Card(
      elevation: 2,
      child: ListTile(
        contentPadding: const EdgeInsets.symmetric(horizontal: 20, vertical: 12),
        leading: Icon(icon, size: 40, color: Theme.of(context).colorScheme.primary),
        title: Text(title, style: const TextStyle(fontWeight: FontWeight.bold, fontSize: 18)),
        subtitle: Text(subtitle),
        trailing: const Icon(Icons.arrow_forward_ios, size: 16),
        onTap: onTap,
      ),
    );
  }
}
