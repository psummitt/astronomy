import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:intl/intl.dart';
import '../../../core/providers/auth_providers.dart';
import '../data/observation_repository.dart';
import '../domain/observation_log.dart';

class LogFormPage extends ConsumerStatefulWidget {
  const LogFormPage({super.key, this.log});

  final ObservationLog? log;

  @override
  ConsumerState<LogFormPage> createState() => _LogFormPageState();
}

class _LogFormPageState extends ConsumerState<LogFormPage> {
  final _formKey = GlobalKey<FormState>();

  late DateTime _date;
  late TimeOfDay _startTime;
  final _targetNameController = TextEditingController();
  final _targetTypeController = TextEditingController();
  final _instrumentNameController = TextEditingController();
  final _weatherConditionsController = TextEditingController();
  final _notesController = TextEditingController();

  @override
  void initState() {
    super.initState();
    final log = widget.log;
    if (log != null) {
      _date = log.observationDate;
      _startTime = TimeOfDay.fromDateTime(log.startTime);
      _targetNameController.text = log.target.name;
      _targetTypeController.text = log.target.objectType ?? '';
      _instrumentNameController.text = log.instrument.name;
      _weatherConditionsController.text = log.weather.conditions;
      _notesController.text = log.notes;
    } else {
      _date = DateTime.now();
      _startTime = TimeOfDay.now();
    }
  }

  @override
  void dispose() {
    _targetNameController.dispose();
    _targetTypeController.dispose();
    _instrumentNameController.dispose();
    _weatherConditionsController.dispose();
    _notesController.dispose();
    super.dispose();
  }

  Future<void> _selectDate() async {
    final picked = await showDatePicker(
      context: context,
      initialDate: _date,
      firstDate: DateTime(1950),
      lastDate: DateTime.now().add(const Duration(days: 1)),
    );
    if (picked != null && picked != _date) {
      setState(() => _date = picked);
    }
  }

  Future<void> _selectTime() async {
    final picked = await showTimePicker(
      context: context,
      initialTime: _startTime,
    );
    if (picked != null && picked != _startTime) {
      setState(() => _startTime = picked);
    }
  }

  void _save() async {
    if (_formKey.currentState!.validate()) {
      final user = ref.read(currentUserProvider);
      if (user == null) {
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
            content: Text('Please sign in or enter guest mode to save logs.'),
          ),
        );
        return;
      }

      final startDateTime = DateTime(
        _date.year,
        _date.month,
        _date.day,
        _startTime.hour,
        _startTime.minute,
      );

      final log = ObservationLog(
        id: widget.log?.id ?? '',
        userId: user.uid,
        observationDate: _date,
        startTime: startDateTime,
        weather: Weather(conditions: _weatherConditionsController.text.trim()),
        target: Target(
          name: _targetNameController.text.trim(),
          objectType: _targetTypeController.text.trim().isNotEmpty
              ? _targetTypeController.text.trim()
              : null,
        ),
        instrument:
            Instrument(name: _instrumentNameController.text.trim()),
        notes: _notesController.text.trim(),
        createdAt: widget.log?.createdAt ?? DateTime.now(),
        updatedAt: DateTime.now(),
      );

      try {
        if (widget.log == null) {
          await ref
              .read(observationRepositoryProvider)
              .createObservation(log);
        } else {
          await ref
              .read(observationRepositoryProvider)
              .updateObservation(log);
        }
        if (mounted) context.pop();
      } catch (e) {
        if (mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            SnackBar(content: Text('Error saving log: $e')),
          );
        }
      }
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(widget.log == null ? 'New Log Entry' : 'Edit Log Entry'),
      ),
      body: Form(
        key: _formKey,
        child: ListView(
          padding: const EdgeInsets.all(16),
          children: [
            Semantics(
              button: true,
              label: 'Selected Date: ${DateFormat.yMMMMd().format(_date)}',
              hint: 'Tap to change observation date',
              child: ListTile(
                title: const Text('Observation Date *'),
                subtitle: Text(DateFormat.yMMMMd().format(_date)),
                trailing: const Icon(Icons.calendar_today),
                shape: RoundedRectangleBorder(
                  side: BorderSide(color: Theme.of(context).dividerColor),
                  borderRadius: BorderRadius.circular(8),
                ),
                onTap: _selectDate,
              ),
            ),
            const SizedBox(height: 12),
            Semantics(
              button: true,
              label: 'Selected Start Time: ${_startTime.format(context)}',
              hint: 'Tap to change observation start time',
              child: ListTile(
                title: const Text('Start Time *'),
                subtitle: Text(_startTime.format(context)),
                trailing: const Icon(Icons.access_time),
                shape: RoundedRectangleBorder(
                  side: BorderSide(color: Theme.of(context).dividerColor),
                  borderRadius: BorderRadius.circular(8),
                ),
                onTap: _selectTime,
              ),
            ),
            const SizedBox(height: 16),
            Semantics(
              label: 'Target Name Input',
              child: TextFormField(
                controller: _targetNameController,
                decoration: const InputDecoration(
                  labelText: 'Target Name * (e.g. M31, Jupiter)',
                  border: OutlineInputBorder(),
                ),
                validator: (value) =>
                    value == null || value.trim().isEmpty ? 'Please enter a target' : null,
              ),
            ),
            const SizedBox(height: 12),
            Semantics(
              label: 'Target Type Input',
              child: TextFormField(
                controller: _targetTypeController,
                decoration: const InputDecoration(
                  labelText: 'Target Object Type (e.g. Spiral Galaxy, Planet)',
                  border: OutlineInputBorder(),
                ),
              ),
            ),
            const SizedBox(height: 12),
            Semantics(
              label: 'Instrument Name Input',
              child: TextFormField(
                controller: _instrumentNameController,
                decoration: const InputDecoration(
                  labelText: 'Instrument * (e.g. 8" Dobsonian, 10x50 Binoculars)',
                  border: OutlineInputBorder(),
                ),
                validator: (value) =>
                    value == null || value.trim().isEmpty ? 'Please enter an instrument' : null,
              ),
            ),
            const SizedBox(height: 12),
            Semantics(
              label: 'Weather Conditions Input',
              child: TextFormField(
                controller: _weatherConditionsController,
                decoration: const InputDecoration(
                  labelText: 'Weather & Sky Conditions * (e.g. Clear, Seeing 4/5)',
                  border: OutlineInputBorder(),
                ),
                validator: (value) =>
                    value == null || value.trim().isEmpty ? 'Please enter weather details' : null,
              ),
            ),
            const SizedBox(height: 12),
            Semantics(
              label: 'Observation Notes Input',
              child: TextFormField(
                controller: _notesController,
                decoration: const InputDecoration(
                  labelText: 'Observation Session Notes',
                  hintText: 'Eyepieces used, visual details observed, filters...',
                  border: OutlineInputBorder(),
                ),
                maxLines: 5,
              ),
            ),
            const SizedBox(height: 24),
            Semantics(
              button: true,
              label: 'Save Observation Log Button',
              child: ElevatedButton.icon(
                onPressed: _save,
                style: ElevatedButton.styleFrom(
                  minimumSize: const Size(double.infinity, 48),
                  padding: const EdgeInsets.symmetric(vertical: 16),
                ),
                icon: const Icon(Icons.save),
                label: const Text('Save Log Entry'),
              ),
            ),
          ],
        ),
      ),
    );
  }
}
