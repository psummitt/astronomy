import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:intl/intl.dart';
import '../data/observation_repository.dart';
import '../domain/observation_log.dart';

class LogDetailPage extends ConsumerWidget {
  const LogDetailPage({super.key, required this.log});

  final ObservationLog log;

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final dateFormat = DateFormat.yMMMMd();
    final timeFormat = DateFormat.jm();

    return Scaffold(
      appBar: AppBar(
        title: Text(log.target.name),
        actions: [
          Semantics(
            button: true,
            label: 'Edit Observation Log Button',
            child: IconButton(
              icon: const Icon(Icons.edit),
              onPressed: () => context.go('/logbook/edit/${log.id}', extra: log),
              tooltip: 'Edit Log',
            ),
          ),
          Semantics(
            button: true,
            label: 'Delete Observation Log Button',
            child: IconButton(
              icon: const Icon(Icons.delete, color: Colors.red),
              onPressed: () => _confirmDelete(context, ref),
              tooltip: 'Delete Log',
            ),
          ),
        ],
      ),
      body: ListView(
        padding: const EdgeInsets.all(16),
        children: [
          _Section(
            title: 'Session Details',
            children: [
              _DetailRow(
                  label: 'Date', value: dateFormat.format(log.observationDate)),
              _DetailRow(
                  label: 'Start Time', value: timeFormat.format(log.startTime)),
              if (log.endTime != null)
                _DetailRow(
                    label: 'End Time', value: timeFormat.format(log.endTime!)),
            ],
          ),
          _Section(
            title: 'Target Parameters',
            children: [
              _DetailRow(label: 'Name', value: log.target.name),
              if (log.target.objectType != null && log.target.objectType!.isNotEmpty)
                _DetailRow(label: 'Type', value: log.target.objectType!),
              if (log.target.catalogId != null && log.target.catalogId!.isNotEmpty)
                _DetailRow(label: 'Catalog ID', value: log.target.catalogId!),
            ],
          ),
          _Section(
            title: 'Equipment & Instrument',
            children: [
              _DetailRow(label: 'Name', value: log.instrument.name),
              if (log.instrument.type != null && log.instrument.type!.isNotEmpty)
                _DetailRow(label: 'Type', value: log.instrument.type!),
              if (log.instrument.apertureMm != null)
                _DetailRow(
                    label: 'Aperture', value: '${log.instrument.apertureMm} mm'),
              if (log.instrument.focalLengthMm != null)
                _DetailRow(
                    label: 'Focal Length', value: '${log.instrument.focalLengthMm} mm'),
            ],
          ),
          _Section(
            title: 'Environmental Conditions',
            children: [
              _DetailRow(label: 'Conditions', value: log.weather.conditions),
              if (log.weather.temperatureC != null)
                _DetailRow(
                    label: 'Temperature', value: '${log.weather.temperatureC} °C'),
              if (log.weather.seeing != null && log.weather.seeing!.isNotEmpty)
                _DetailRow(label: 'Seeing', value: log.weather.seeing!),
              if (log.weather.transparency != null && log.weather.transparency!.isNotEmpty)
                _DetailRow(label: 'Transparency', value: log.weather.transparency!),
            ],
          ),
          if (log.notes.isNotEmpty)
            _Section(
              title: 'Observation Notes',
              children: [
                Semantics(
                  label: 'Full observation session notes',
                  child: Text(
                    log.notes,
                    style: Theme.of(context).textTheme.bodyLarge,
                  ),
                ),
              ],
            ),
        ],
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
      await ref.read(observationRepositoryProvider).deleteObservation(log.id);
      if (context.mounted) context.go('/logbook');
    }
  }
}

class _Section extends StatelessWidget {
  const _Section({required this.title, required this.children});

  final String title;
  final List<Widget> children;

  @override
  Widget build(BuildContext context) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Padding(
          padding: const EdgeInsets.symmetric(vertical: 8),
          child: Text(
            title,
            style: Theme.of(context).textTheme.titleMedium?.copyWith(
                  color: Theme.of(context).colorScheme.primary,
                  fontWeight: FontWeight.bold,
                ),
          ),
        ),
        ...children,
        const Divider(),
      ],
    );
  }
}

class _DetailRow extends StatelessWidget {
  const _DetailRow({required this.label, required this.value});

  final String label;
  final String value;

  @override
  Widget build(BuildContext context) {
    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 4),
      child: Row(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          SizedBox(
            width: 120,
            child: Text(
              label,
              style: const TextStyle(fontWeight: FontWeight.w600),
            ),
          ),
          Expanded(child: Text(value)),
        ],
      ),
    );
  }
}
