
import 'package:flutter/material.dart';
import 'package:intl/intl.dart';
import 'polaris_calculator.dart';
import 'help_screen.dart';

void main() {
  runApp(const PStarApp());
}

class PStarApp extends StatelessWidget {
  const PStarApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'PStar Polaris Positions',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.deepPurple, brightness: Brightness.dark),
        useMaterial3: true,
      ),
      home: const MainScreen(),
    );
  }
}

class MainScreen extends StatefulWidget {
  const MainScreen({super.key});

  @override
  State<MainScreen> createState() => _MainScreenState();
}

class _MainScreenState extends State<MainScreen> {
  final _formKey = GlobalKey<FormState>();
  DateTime _selectedDate = DateTime.now();
  final _longitudeController = TextEditingController(text: '0.0');
  PolarisEventType _selectedEvent = PolarisEventType.westElongationNext;
  PolarisResult? _result;

  void _calculate() {
    if (_formKey.currentState!.validate()) {
      setState(() {
        _result = PolarisCalculator.calculate(
          year: _selectedDate.year,
          month: _selectedDate.month,
          day: _selectedDate.day,
          longitude: double.tryParse(_longitudeController.text) ?? 0.0,
          eventType: _selectedEvent,
        );
      });
    }
  }

  Future<void> _selectDate(BuildContext context) async {
    final DateTime? picked = await showDatePicker(
      context: context,
      initialDate: _selectedDate,
      firstDate: DateTime(1900),
      lastDate: DateTime(2100),
    );
    if (picked != null && picked != _selectedDate) {
      setState(() {
        _selectedDate = picked;
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('PStar Polaris Positions'),
        actions: [
          IconButton(
            icon: const Icon(Icons.help_outline),
            onPressed: () {
              Navigator.push(
                context,
                MaterialPageRoute(builder: (context) => const HelpScreen()),
              );
            },
            tooltip: 'Show Help',
          ),
        ],
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16.0),
        child: Form(
          key: _formKey,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              Semantics(
                header: true,
                child: Text(
                  'Astronomy Program by Eric Burgess',
                  style: Theme.of(context).textTheme.titleMedium,
                  textAlign: TextAlign.center,
                ),
              ),
              const SizedBox(height: 20),
              ListTile(
                title: Text("Date: ${DateFormat('yyyy-MM-dd').format(_selectedDate)}"),
                trailing: const Icon(Icons.calendar_today),
                onTap: () => _selectDate(context),
              ),
              const SizedBox(height: 10),
              TextFormField(
                controller: _longitudeController,
                decoration: const InputDecoration(
                  labelText: 'Longitude (Degrees)',
                  border: OutlineInputBorder(),
                  hintText: 'e.g. 122.3',
                ),
                keyboardType: const TextInputType.numberWithOptions(decimal: true),
                validator: (value) {
                  if (value == null || value.isEmpty) {
                    return 'Please enter longitude';
                  }
                  if (double.tryParse(value) == null) {
                    return 'Please enter a valid number';
                  }
                  return null;
                },
              ),
              const SizedBox(height: 20),
              Text(
                'Select Event:',
                style: Theme.of(context).textTheme.titleMedium,
              ),
              ...PolarisEventType.values.map((type) {
                return RadioListTile<PolarisEventType>(
                  title: Text(_getEventName(type)),
                  value: type,
                  groupValue: _selectedEvent,
                  onChanged: (PolarisEventType? value) {
                    setState(() {
                      _selectedEvent = value!;
                    });
                  },
                );
              }),
              const SizedBox(height: 20),
              ElevatedButton(
                onPressed: _calculate,
                child: const Text('Calculate'),
              ),
              if (_result != null) ...[
                const SizedBox(height: 30),
                Card(
                  child: Padding(
                    padding: const EdgeInsets.all(16.0),
                    child: Column(
                      children: [
                        Text(
                          'Result for ${DateFormat('yyyy-MM-dd').format(_selectedDate)}',
                          style: Theme.of(context).textTheme.titleSmall,
                        ),
                        const SizedBox(height: 10),
                        Semantics(
                          liveRegion: true,
                          child: Text(
                            _result.toString(),
                            style: Theme.of(context).textTheme.headlineSmall?.copyWith(
                                  color: Theme.of(context).colorScheme.primary,
                                  fontWeight: FontWeight.bold,
                                ),
                            textAlign: TextAlign.center,
                          ),
                        ),
                      ],
                    ),
                  ),
                ),
              ],
            ],
          ),
        ),
      ),
    );
  }

  String _getEventName(PolarisEventType type) {
    switch (type) {
      case PolarisEventType.westElongationNext:
        return 'West Elongation Next';
      case PolarisEventType.westElongationPrevious:
        return 'West Elongation Previous';
      case PolarisEventType.eastElongationNext:
        return 'East Elongation Next';
      case PolarisEventType.eastElongationPrevious:
        return 'East Elongation Previous';
      case PolarisEventType.lowerTransitNext:
        return 'Lower Transit Next';
      case PolarisEventType.upperTransitNext:
        return 'Upper Transit Next';
    }
  }
}
