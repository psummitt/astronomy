import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:intl/intl.dart';
import '../data/csv_handler.dart';
import '../data/plan_provider.dart';

class PlannerListPage extends ConsumerWidget {
  const PlannerListPage({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final observations = ref.watch(planProvider);

    return Scaffold(
      appBar: AppBar(
        title: const Text('Observation Planner'),
        actions: [
          Semantics(
            label: 'Import or Export CSV Plan Options',
            button: true,
            child: PopupMenuButton<String>(
              icon: const Icon(Icons.more_vert),
              tooltip: 'CSV Options',
              onSelected: (value) async {
                if (value == 'export') {
                  if (observations.isEmpty) {
                    ScaffoldMessenger.of(context).showSnackBar(
                      const SnackBar(
                        content: Text('Plan is empty. Nothing to export.'),
                        behavior: SnackBarBehavior.floating,
                      ),
                    );
                    return;
                  }
                  final success = await CsvHandler.exportToCsv(observations);
                  if (context.mounted && success) {
                    ScaffoldMessenger.of(context).showSnackBar(
                      const SnackBar(
                        content: Text('Observation plan exported successfully.'),
                        behavior: SnackBarBehavior.floating,
                      ),
                    );
                  }
                } else if (value == 'import') {
                  final imported = await CsvHandler.importFromCsv();
                  if (imported != null && imported.isNotEmpty) {
                    await ref.read(planProvider.notifier).importObservations(imported);
                    if (context.mounted) {
                      ScaffoldMessenger.of(context).showSnackBar(
                        SnackBar(
                          content: Text('Imported ${imported.length} target(s).'),
                          behavior: SnackBarBehavior.floating,
                        ),
                      );
                    }
                  }
                }
              },
              itemBuilder: (context) => [
                const PopupMenuItem(
                  value: 'export',
                  child: Row(
                    children: [
                      Icon(Icons.download_outlined, size: 20),
                      SizedBox(width: 8),
                      Text('Export CSV Plan'),
                    ],
                  ),
                ),
                const PopupMenuItem(
                  value: 'import',
                  child: Row(
                    children: [
                      Icon(Icons.upload_outlined, size: 20),
                      SizedBox(width: 8),
                      Text('Import CSV Plan'),
                    ],
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
      body: observations.isEmpty
          ? Center(
              child: Padding(
                padding: const EdgeInsets.all(24.0),
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    Icon(
                      Icons.auto_awesome_outlined,
                      size: 64,
                      color: Theme.of(context).colorScheme.primary.withValues(alpha: 0.6),
                    ),
                    const SizedBox(height: 16),
                    Text(
                      'No planned targets yet',
                      style: Theme.of(context).textTheme.headlineSmall,
                    ),
                    const SizedBox(height: 8),
                    Text(
                      'Create an observation target plan with coordinates and notes.',
                      textAlign: TextAlign.center,
                      style: Theme.of(context).textTheme.bodyMedium,
                    ),
                    const SizedBox(height: 24),
                    Semantics(
                      button: true,
                      label: 'Add First Planned Target',
                      child: ElevatedButton.icon(
                        style: ElevatedButton.styleFrom(
                          minimumSize: const Size(200, 48),
                        ),
                        onPressed: () => context.go('/planner/add'),
                        icon: const Icon(Icons.add),
                        label: const Text('Add Target Plan'),
                      ),
                    ),
                  ],
                ),
              ),
            )
          : ListView.builder(
              itemCount: observations.length,
              itemBuilder: (context, index) {
                final obs = observations[index];
                final formattedDate =
                    DateFormat('E, MMM d yyyy • HH:mm').format(obs.dateTime);

                return Semantics(
                  label:
                      'Target ${obs.targetName}, scheduled for $formattedDate, Right Ascension ${obs.ra}, Declination ${obs.dec}',
                  child: Card(
                    child: ListTile(
                      contentPadding: const EdgeInsets.symmetric(
                          horizontal: 16, vertical: 8),
                      title: Text(
                        obs.targetName,
                        style: Theme.of(context).textTheme.titleLarge?.copyWith(
                              fontWeight: FontWeight.bold,
                            ),
                      ),
                      subtitle: Column(
                        crossAxisAlignment: CrossAxisAlignment.start,
                        children: [
                          const SizedBox(height: 4),
                          Row(
                            children: [
                              const Icon(Icons.schedule, size: 16),
                              const SizedBox(width: 4),
                              Text(formattedDate),
                            ],
                          ),
                          const SizedBox(height: 4),
                          Text('RA: ${obs.ra} | Dec: ${obs.dec}'),
                          if (obs.notes.isNotEmpty) ...[
                            const SizedBox(height: 4),
                            Text(
                              'Notes: ${obs.notes}',
                              maxLines: 2,
                              overflow: TextOverflow.ellipsis,
                              style: TextStyle(
                                color: Theme.of(context)
                                    .colorScheme
                                    .onSurface
                                    .withValues(alpha: 0.75),
                              ),
                            ),
                          ],
                        ],
                      ),
                      trailing: Semantics(
                        button: true,
                        label: 'Delete planned target ${obs.targetName}',
                        child: IconButton(
                          icon: const Icon(Icons.delete_outline, color: Colors.red),
                          tooltip: 'Delete Target',
                          onPressed: () => _confirmDelete(context, ref, obs.id, obs.targetName),
                        ),
                      ),
                    ),
                  ),
                );
              },
            ),
      floatingActionButton: Semantics(
        button: true,
        label: 'Add New Planned Target',
        child: FloatingActionButton.extended(
          onPressed: () => context.go('/planner/add'),
          tooltip: 'Add Target Plan',
          icon: const Icon(Icons.add),
          label: const Text('Add Plan'),
        ),
      ),
    );
  }

  Future<void> _confirmDelete(
      BuildContext context, WidgetRef ref, int? id, String name) async {
    final confirmed = await showDialog<bool>(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Delete Target Plan'),
        content: Text('Are you sure you want to delete "$name" from your planner?'),
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

    if (confirmed == true && id != null) {
      await ref.read(planProvider.notifier).deleteObservation(id);
    }
  }
}
