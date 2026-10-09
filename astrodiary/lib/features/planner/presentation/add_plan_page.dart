import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:intl/intl.dart';

import '../../../services/moon_calculator.dart';
import '../../../services/planet_calculator.dart';
import '../../../services/star_service.dart';
import '../data/plan_provider.dart';
import '../domain/planner_observation.dart';

class AddPlanPage extends ConsumerStatefulWidget {
  final DateTime? initialDate;

  const AddPlanPage({super.key, this.initialDate});

  @override
  ConsumerState<AddPlanPage> createState() => _AddPlanPageState();
}

class _AddPlanPageState extends ConsumerState<AddPlanPage> {
  final _formKey = GlobalKey<FormState>();
  final _targetNameController = TextEditingController();
  final _raController = TextEditingController();
  final _decController = TextEditingController();
  final _notesController = TextEditingController();
  
  late DateTime _selectedDate;
  bool _isCalculating = false;
  String? _calculationStatus;

  @override
  void initState() {
    super.initState();
    _selectedDate = widget.initialDate ?? DateTime.now().add(const Duration(hours: 2));
  }

  @override
  void dispose() {
    _targetNameController.dispose();
    _raController.dispose();
    _decController.dispose();
    _notesController.dispose();
    super.dispose();
  }

  Future<void> _calculateCoordinates() async {
    final name = _targetNameController.text.trim();
    if (name.isEmpty) return;

    setState(() {
      _isCalculating = true;
      _calculationStatus = null;
    });

    final nameLower = name.toLowerCase();

    if (nameLower == 'moon') {
      final pos = MoonCalculator.calculateForDateTime(_selectedDate, mode: MoonCalculationMode.modern);
      _raController.text = pos.raFormatted;
      _decController.text = pos.decFormatted;
      setState(() {
        _isCalculating = false;
        _calculationStatus = 'Calculated via RADEM Modern Lunar Model';
      });
      return;
    }

    if (PlanetCalculator.isPlanet(name)) {
      final pos = PlanetCalculator.calculateForPlanet(name, _selectedDate);
      if (pos != null) {
        _raController.text = pos.raFormatted;
        _decController.text = pos.decFormatted;
        setState(() {
          _isCalculating = false;
          _calculationStatus = 'Calculated via RADEM Planetary Model';
        });
        return;
      }
    }

    // Try star search catalog
    try {
      final star = await StarService.fetchStarData(name);
      if (star != null) {
        _raController.text = star.raFormatted;
        _decController.text = star.decFormatted;
        setState(() {
          _isCalculating = false;
          _calculationStatus = 'Resolved via CDS Strasbourg Star Catalog';
        });
        return;
      }
    } catch (_) {}

    setState(() {
      _isCalculating = false;
      _calculationStatus = 'Object coordinates not found automatically. Enter RA/Dec manually.';
    });
  }

  Future<void> _pickDateTime() async {
    final DateTime? pickedDate = await showDatePicker(
      context: context,
      initialDate: _selectedDate,
      firstDate: DateTime.now().subtract(const Duration(days: 365)),
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
        _calculateCoordinates();
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
              TextFormField(
                controller: _targetNameController,
                decoration: InputDecoration(
                  labelText: 'Target Name *',
                  hintText: 'e.g. Moon, Jupiter, Sirius, M31',
                  border: const OutlineInputBorder(),
                  suffixIcon: IconButton(
                    icon: const Icon(Icons.auto_awesome),
                    tooltip: 'Auto-calculate RA/Dec for Moon, Planets, or Stars',
                    onPressed: _calculateCoordinates,
                  ),
                ),
                onChanged: (_) {
                  _calculateCoordinates();
                },
                validator: (value) =>
                    (value == null || value.trim().isEmpty)
                        ? 'Please enter a target name'
                        : null,
              ),
              if (_isCalculating)
                const Padding(
                  padding: EdgeInsets.only(top: 8.0),
                  child: LinearProgressIndicator(),
                ),
              if (_calculationStatus != null)
                Padding(
                  padding: const EdgeInsets.only(top: 6.0, bottom: 4.0),
                  child: Text(
                    _calculationStatus!,
                    style: TextStyle(
                      fontSize: 12,
                      color: _calculationStatus!.startsWith('Calculated') ||
                              _calculationStatus!.startsWith('Resolved')
                          ? Colors.green
                          : Colors.orange,
                    ),
                  ),
                ),
              const SizedBox(height: 16),
              ListTile(
                title: const Text('Scheduled Observation Time'),
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
              const SizedBox(height: 16),
              Row(
                children: [
                  Expanded(
                    child: TextFormField(
                      controller: _raController,
                      decoration: const InputDecoration(
                        labelText: 'RA (Right Ascension) *',
                        hintText: 'e.g. 14h 15m 00s',
                        border: OutlineInputBorder(),
                      ),
                      validator: (value) =>
                          (value == null || value.trim().isEmpty)
                              ? 'Required'
                              : null,
                    ),
                  ),
                  const SizedBox(width: 16),
                  Expanded(
                    child: TextFormField(
                      controller: _decController,
                      decoration: const InputDecoration(
                        labelText: 'Dec (Declination) *',
                        hintText: 'e.g. +12° 30\' 00"',
                        border: OutlineInputBorder(),
                      ),
                      validator: (value) =>
                          (value == null || value.trim().isEmpty)
                              ? 'Required'
                              : null,
                    ),
                  ),
                ],
              ),
              const SizedBox(height: 16),
              TextFormField(
                controller: _notesController,
                maxLines: 4,
                decoration: const InputDecoration(
                  labelText: 'Planning Notes',
                  hintText: 'Equipment requirements, filters, target goals...',
                  border: OutlineInputBorder(),
                ),
              ),
              const SizedBox(height: 24),
              ElevatedButton.icon(
                onPressed: _save,
                style: ElevatedButton.styleFrom(
                  padding: const EdgeInsets.symmetric(vertical: 16),
                  minimumSize: const Size(double.infinity, 48),
                ),
                icon: const Icon(Icons.save),
                label: const Text('Save Target Plan'),
              ),
            ],
          ),
        ),
      ),
    );
  }
}
