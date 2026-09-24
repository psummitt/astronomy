import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:intl/intl.dart';
import '../../../core/providers/auth_providers.dart';
import '../../../core/providers/firebase_providers.dart';
import '../data/observation_repository.dart';
import '../domain/observation_log.dart';

class LogListPage extends ConsumerWidget {
  const LogListPage({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final user = ref.watch(currentUserProvider);
    final logsAsync = ref.watch(observationsStreamProvider);

    return Scaffold(
      appBar: AppBar(
        title: const Text('Observation Logbook'),
        actions: [
          if (user != null)
            Semantics(
              button: true,
              label: 'Sign Out Button',
              child: IconButton(
                icon: const Icon(Icons.logout),
                onPressed: () => ref.read(firebaseAuthProvider).signOut(),
                tooltip: 'Sign Out',
              ),
            )
          else
            Semantics(
              button: true,
              label: 'Sign In Button',
              child: IconButton(
                icon: const Icon(Icons.login),
                onPressed: () => context.go('/login'),
                tooltip: 'Sign In',
              ),
            ),
        ],
      ),
      body: user == null
          ? Center(
              child: Padding(
                padding: const EdgeInsets.all(24.0),
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    Icon(
                      Icons.account_circle_outlined,
                      size: 64,
                      color: Theme.of(context).colorScheme.primary.withValues(alpha: 0.7),
                    ),
                    const SizedBox(height: 16),
                    Text(
                      'Observation Log Sync',
                      style: Theme.of(context).textTheme.headlineSmall,
                    ),
                    const SizedBox(height: 8),
                    const Text(
                      'Sign in or join as guest to record and sync your night sky observation logs across device targets.',
                      textAlign: TextAlign.center,
                    ),
                    const SizedBox(height: 24),
                    Semantics(
                      button: true,
                      label: 'Go to Sign In Screen',
                      child: ElevatedButton.icon(
                        style: ElevatedButton.styleFrom(
                          minimumSize: const Size(200, 48),
                        ),
                        onPressed: () => context.go('/login'),
                        icon: const Icon(Icons.login),
                        label: const Text('Sign In / Guest Access'),
                      ),
                    ),
                  ],
                ),
              ),
            )
          : logsAsync.when(
              data: (logs) => logs.isEmpty
                  ? Center(
                      child: Padding(
                        padding: const EdgeInsets.all(24.0),
                        child: Column(
                          mainAxisAlignment: MainAxisAlignment.center,
                          children: [
                            const Icon(Icons.menu_book, size: 64, color: Colors.grey),
                            const SizedBox(height: 16),
                            Text(
                              'No observation logs recorded yet',
                              style: Theme.of(context).textTheme.headlineSmall,
                            ),
                            const SizedBox(height: 8),
                            const Text(
                              'Tap below to record details of your telescopic observation session.',
                              textAlign: TextAlign.center,
                            ),
                            const SizedBox(height: 24),
                            Semantics(
                              button: true,
                              label: 'Log New Observation Session',
                              child: ElevatedButton.icon(
                                style: ElevatedButton.styleFrom(
                                  minimumSize: const Size(200, 48),
                                ),
                                onPressed: () => context.go('/logbook/add'),
                                icon: const Icon(Icons.add),
                                label: const Text('Log New Observation'),
                              ),
                            ),
                          ],
                        ),
                      ),
                    )
                  : ListView.builder(
                      itemCount: logs.length,
                      itemBuilder: (context, index) {
                        final log = logs[index];
                        return ObservationLogCard(log: log);
                      },
                    ),
              loading: () => const Center(
                child: CircularProgressIndicator(),
              ),
              error: (err, stack) => Center(
                child: Padding(
                  padding: const EdgeInsets.all(24.0),
                  child: Column(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: [
                      const Icon(Icons.error_outline, color: Colors.red, size: 48),
                      const SizedBox(height: 16),
                      Text('Error loading observation logs: $err'),
                    ],
                  ),
                ),
              ),
            ),
      floatingActionButton: user != null
          ? Semantics(
              button: true,
              label: 'Add Observation Log Floating Action Button',
              child: FloatingActionButton.extended(
                onPressed: () => context.go('/logbook/add'),
                tooltip: 'Add Observation Log',
                icon: const Icon(Icons.add),
                label: const Text('New Log'),
              ),
            )
          : null,
    );
  }
}

class ObservationLogCard extends ConsumerWidget {
  const ObservationLogCard({super.key, required this.log});

  final ObservationLog log;

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final dateFormat = DateFormat.yMMMMd();
    final timeFormat = DateFormat.jm();

    return Semantics(
      label:
          'Observation log for ${log.target.name} on ${dateFormat.format(log.observationDate)}. Instrument: ${log.instrument.name}. Weather: ${log.weather.conditions}',
      child: Card(
        child: ListTile(
          contentPadding:
              const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
          title: Text(
            log.target.name,
            style: Theme.of(context).textTheme.titleLarge?.copyWith(
                  fontWeight: FontWeight.bold,
                ),
          ),
          subtitle: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              const SizedBox(height: 4),
              Text(
                '${dateFormat.format(log.observationDate)} at ${timeFormat.format(log.startTime)}',
              ),
              Text('Instrument: ${log.instrument.name}'),
              Text('Weather: ${log.weather.conditions}'),
            ],
          ),
          trailing: Row(
            mainAxisSize: MainAxisSize.min,
            children: [
              Semantics(
                button: true,
                label: 'Delete observation log for ${log.target.name}',
                child: IconButton(
                  icon: const Icon(Icons.delete_outline, color: Colors.red),
                  tooltip: 'Delete Log',
                  onPressed: () => _confirmDelete(context, ref),
                ),
              ),
              const Icon(Icons.chevron_right),
            ],
          ),
          onTap: () => context.go('/logbook/detail/${log.id}', extra: log),
        ),
      ),
    );
  }

  Future<void> _confirmDelete(BuildContext context, WidgetRef ref) async {
    final confirmed = await showDialog<bool>(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Delete Observation Log'),
        content: Text(
            'Are you sure you want to delete the log for "${log.target.name}"?'),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context, false),
            child: const Text('Cancel'),
          ),
          TextButton(
            onPressed: () => Navigator.pop(context, true),
            style: TextButton.styleFrom(foregroundColor: Colors.red),
            child: const Text('Delete'),
          ),
        ],
      ),
    );

    if (confirmed == true) {
      await ref
          .read(observationRepositoryProvider)
          .deleteObservation(log.id);
    }
  }
}
