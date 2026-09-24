import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:intl/intl.dart';
import '../data/plan_provider.dart';
import '../domain/planner_observation.dart';

class AddPlanPage extends ConsumerStatefulWidget {
  const AddPlanPage({super.key});

  @override
  ConsumerState<AddPlanPage> createState() => _AddPlanPageState();
}

class _AddPlanPageState extends ConsumerState<AddPlanPage> {
  final _formKey = GlobalKey<FormState>();
  final _targetNameController = TextEditingController();
  final _raController = TextEditingController();
  final _decController = TextEditingController();
  final _notesController = TextEditingController();
  DateTime _selectedDate = DateTime.now().add(const Duration(hours: 2));

  @override
  void dispose() {
    _targetNameController.dispose();
    _raController.dispose();
    _decController.dispose();
    _notesController.dispose();
    super.dispose();
  }

  Future<void> _pickDateTime() async {
    final DateTime? pickedDate = await showDatePicker(
      context: context,
      initialDate: _selectedDate,
      firstDate: DateTime.now().subtract(const Duration(days: 1)),
      lastDate: DateTime(2100),
    );
    if (pickedDate != null) {
      if (!mounted) return;
      final TimeOfDay? pickedTime = await showTimePicker(
        context: context,
        initialTime: TimeOfDay.fromDateTime(_selectedDate),
      );
      if (pickedTime != null) {
        setState(() {
          _selectedDate = DateTime(
            pickedDate.year,
            pickedDate.month,
            pickedDate.day,
            pickedTime.hour,
            pickedTime.minute,
          );
        });
      }
    }
  }

  void _save() {
    if (_formKey.currentState!.validate()) {
      final newObs = PlannerObservation(
        targetName: _targetNameController.text.trim(),
        dateTime: _selectedDate,
        ra: _raController.text.trim(),
        dec: _decController.text.trim(),
        notes: _notesController.text.trim(),
      );
      ref.read(planProvider.notifier).addObservation(newObs);
      context.pop();
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Add Target Plan'),
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16.0),
        child: Form(
          key: _formKey,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              Semantics(
                label: 'Target Name Input',
                hint: 'Enter astronomical object name such as M31 or Saturn',
                child: TextFormField(
                  controller: _targetNameController,
                  decoration: const InputDecoration(
                    labelText: 'Target Name *',
                    hintText: 'e.g. Andromeda Galaxy, Saturn, M42',
                    border: OutlineInputBorder(),
                  ),
                  validator: (value) =>
                      (value == null || value.trim().isEmpty)
                          ? 'Please enter a target name'
                          : null,
                ),
              ),
              const SizedBox(height: 16),
              Semantics(
                button: true,
                label: 'Selected Observation Date and Time: ${DateFormat('yyyy-MM-dd HH:mm').format(_selectedDate)}',
                hint: 'Tap to change observation date and time',
                child: ListTile(
                  title: const Text('Scheduled Time'),
                  subtitle: Text(
                    DateFormat('yyyy-MM-dd HH:mm').format(_selectedDate),
                    style: Theme.of(context).textTheme.titleMedium,
                  ),
                  trailing: const Icon(Icons.calendar_today),
                  shape: RoundedRectangleBorder(
                    side: BorderSide(color: Theme.of(context).dividerColor),
                    borderRadius: BorderRadius.circular(8),
                  ),
                  onTap: _pickDateTime,
                ),
              ),
              const SizedBox(height: 16),
              Row(
                children: [
                  Expanded(
                    child: Semantics(
                      label: 'Right Ascension Input',
                      child: TextFormField(
                        controller: _raController,
                        decoration: const InputDecoration(
                          labelText: 'RA (Right Ascension) *',
                          hintText: 'e.g. 00h 42m 44s',
                          border: OutlineInputBorder(),
                        ),
                        validator: (value) =>
                            (value == null || value.trim().isEmpty)
                                ? 'Required'
                                : null,
                      ),
                    ),
                  ),
                  const SizedBox(width: 16),
                  Expanded(
                    child: Semantics(
                      label: 'Declination Input',
                      child: TextFormField(
                        controller: _decController,
                        decoration: const InputDecoration(
                          labelText: 'Dec (Declination) *',
                          hintText: 'e.g. +41° 16\' 09"',
                          border: OutlineInputBorder(),
                        ),
                        validator: (value) =>
                            (value == null || value.trim().isEmpty)
                                ? 'Required'
                                : null,
                      ),
                    ),
                  ),
                ],
              ),
              const SizedBox(height: 16),
              Semantics(
                label: 'Plan Notes Input',
                child: TextFormField(
                  controller: _notesController,
                  maxLines: 4,
                  decoration: const InputDecoration(
                    labelText: 'Planning Notes',
                    hintText: 'Equipment requirements, filters, goals, best viewing time...',
                    border: OutlineInputBorder(),
                  ),
                ),
              ),
              const SizedBox(height: 24),
              Semantics(
                button: true,
                label: 'Save Target Plan Button',
                child: ElevatedButton.icon(
                  onPressed: _save,
                  style: ElevatedButton.styleFrom(
                    padding: const EdgeInsets.symmetric(vertical: 16),
                    minimumSize: const Size(double.infinity, 48),
                  ),
                  icon: const Icon(Icons.save),
                  label: const Text('Save Target Plan'),
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }
}
