import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import '../../../app/theme.dart';
import '../../../core/providers/auth_providers.dart';
import '../../../core/providers/firebase_providers.dart';
import '../../../core/providers/theme_provider.dart';

class SettingsPage extends ConsumerWidget {
  const SettingsPage({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final currentTheme = ref.watch(themeProvider);
    final user = ref.watch(currentUserProvider);

    return Scaffold(
      appBar: AppBar(
        title: const Text('Settings & Profile'),
      ),
      body: ListView(
        padding: const EdgeInsets.all(16),
        children: [
          _buildSectionHeader(context, 'Account Profile'),
          Card(
            child: ListTile(
              leading: const Icon(Icons.account_circle, size: 36),
              title: Text(
                user == null
                    ? 'Not Signed In'
                    : user.isAnonymous
                        ? 'Guest User'
                        : user.email ?? 'Signed In User',
                style: const TextStyle(fontWeight: FontWeight.bold),
              ),
              subtitle: Text(
                user == null
                    ? 'Sign in to back up and sync observation logs.'
                    : 'UID: ${user.uid}',
              ),
              trailing: user == null
                  ? ElevatedButton(
                      onPressed: () => context.go('/login'),
                      child: const Text('Sign In'),
                    )
                  : OutlinedButton(
                      onPressed: () =>
                          ref.read(firebaseAuthProvider).signOut(),
                      child: const Text('Sign Out'),
                    ),
            ),
          ),
          const SizedBox(height: 16),
          _buildSectionHeader(context, 'Display & Field Modes'),
          Card(
            child: Column(
              children: [
                RadioListTile<AppThemeMode>(
                  title: const Text('Dark Space Theme'),
                  subtitle: const Text('Default deep navy/black background for general nighttime viewing.'),
                  value: AppThemeMode.dark,
                  groupValue: currentTheme,
                  onChanged: (mode) {
                    if (mode != null) {
                      ref.read(themeProvider.notifier).setThemeMode(mode);
                    }
                  },
                ),
                const Divider(height: 1),
                RadioListTile<AppThemeMode>(
                  title: const Row(
                    children: [
                      Icon(Icons.remove_red_eye, color: Colors.red),
                      SizedBox(width: 8),
                      Text('Red Night Vision Mode'),
                    ],
                  ),
                  subtitle: const Text('High-contrast pure red on pitch black UI to preserve dark adaptation at field sites.'),
                  value: AppThemeMode.redNightVision,
                  groupValue: currentTheme,
                  onChanged: (mode) {
                    if (mode != null) {
                      ref.read(themeProvider.notifier).setThemeMode(mode);
                    }
                  },
                ),
                const Divider(height: 1),
                RadioListTile<AppThemeMode>(
                  title: const Text('Light Theme'),
                  subtitle: const Text('Standard light background for daytime log editing.'),
                  value: AppThemeMode.light,
                  groupValue: currentTheme,
                  onChanged: (mode) {
                    if (mode != null) {
                      ref.read(themeProvider.notifier).setThemeMode(mode);
                    }
                  },
                ),
              ],
            ),
          ),
          const SizedBox(height: 16),
          _buildSectionHeader(context, 'App Information'),
          ListTile(
            leading: const Icon(Icons.help_outline),
            title: const Text('Instructions & How-To Guide'),
            trailing: const Icon(Icons.chevron_right),
            onTap: () => context.go('/instructions'),
          ),
          ListTile(
            leading: const Icon(Icons.info_outline),
            title: const Text('About AstroDiary'),
            trailing: const Icon(Icons.chevron_right),
            onTap: () => context.go('/about'),
          ),
        ],
      ),
    );
  }

  Widget _buildSectionHeader(BuildContext context, String title) {
    return Padding(
      padding: const EdgeInsets.only(left: 4, bottom: 8, top: 8),
      child: Text(
        title,
        style: Theme.of(context).textTheme.titleSmall?.copyWith(
              color: Theme.of(context).colorScheme.primary,
              fontWeight: FontWeight.bold,
            ),
      ),
    );
  }
}
