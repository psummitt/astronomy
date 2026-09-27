import 'package:flutter/material.dart';
import '../logic/legacy_calculator.dart';
import '../logic/modern_calculator.dart';
import '../logic/base_calculator.dart';
import 'results_screen.dart';
import 'help_screen.dart';

class HomeScreen extends StatefulWidget {
  const HomeScreen({super.key});

  @override
  State<HomeScreen> createState() => _HomeScreenState();
}

class _HomeScreenState extends State<HomeScreen> {
  final TextEditingController _yearController = TextEditingController(text: DateTime.now().year.toString());
  final TextEditingController _latController = TextEditingController();
  final TextEditingController _lonController = TextEditingController();
  bool _useLegacy = false;
  bool _filterByLocation = false;
  DateTime _selectedDate = DateTime.now();

  void _calculate() {
    int? year = int.tryParse(_yearController.text);
    if (year == null) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(content: Text('Please enter a valid year')),
      );
      return;
    }

    double? userLat = double.tryParse(_latController.text);
    double? userLon = double.tryParse(_lonController.text);

    if (_filterByLocation && (userLat == null || userLon == null)) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(content: Text('Please enter valid coordinates for filtering')),
      );
      return;
    }

    BaseCalculator calculator = _useLegacy ? LegacyCalculator() : ModernCalculator();
    
    Navigator.push(
      context,
      MaterialPageRoute(
        builder: (context) => ResultsScreen(
          startYear: year,
          calculator: calculator,
          userLat: _filterByLocation ? userLat : null,
          userLon: _filterByLocation ? userLon : null,
        ),
      ),
    );
  }

  Future<void> _selectDate(BuildContext context) async {
    showDialog(
      context: context,
      builder: (BuildContext context) {
        return AlertDialog(
          title: const Text("Select Year"),
          content: SizedBox(
            width: 300,
            height: 300,
            child: YearPicker(
              firstDate: DateTime(1900),
              lastDate: DateTime(2100),
              selectedDate: _selectedDate,
              onChanged: (DateTime dateTime) {
                setState(() {
                  _selectedDate = dateTime;
                  _yearController.text = dateTime.year.toString();
                });
                Navigator.pop(context);
              },
            ),
          ),
        );
      },
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Lunar Eclipse Calculator'),
        actions: [
          IconButton(
            icon: const Icon(Icons.help_outline),
            onPressed: () => Navigator.push(
              context,
              MaterialPageRoute(builder: (context) => const HelpScreen()),
            ),
            tooltip: 'Help and Instructions',
          ),
        ],
      ),
      body: Center(
        child: SingleChildScrollView(
          padding: const EdgeInsets.all(24.0),
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              const Icon(Icons.nightlight_round, size: 80, color: Colors.indigo),
              const SizedBox(height: 16),
              Text(
                'LUNAR UMBRAL ECLIPSES',
                style: Theme.of(context).textTheme.headlineMedium?.copyWith(
                  fontWeight: FontWeight.bold,
                  letterSpacing: 1.2,
                ),
                textAlign: TextAlign.center,
              ),
              const SizedBox(height: 32),
              
              // Year Input with Date Picker
              Row(
                mainAxisSize: MainAxisSize.min,
                children: [
                  SizedBox(
                    width: 150,
                    child: TextField(
                      controller: _yearController,
                      decoration: const InputDecoration(
                        labelText: 'Starting Year',
                        border: OutlineInputBorder(),
                        prefixIcon: Icon(Icons.calendar_today),
                      ),
                      keyboardType: TextInputType.number,
                    ),
                  ),
                  const SizedBox(width: 8),
                  IconButton.filledTonal(
                    onPressed: () => _selectDate(context),
                    icon: const Icon(Icons.date_range),
                    tooltip: 'Open Calendar',
                  ),
                ],
              ),
              
              const SizedBox(height: 24),
              
              // Location Filtering Section
              Card(
                child: Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Column(
                    children: [
                      Row(
                        children: [
                          const Icon(Icons.location_on, color: Colors.indigo),
                          const SizedBox(width: 8),
                          const Text('Filter by Visibility', style: TextStyle(fontWeight: FontWeight.bold)),
                          const Spacer(),
                          Switch(
                            value: _filterByLocation,
                            onChanged: (val) => setState(() => _filterByLocation = val),
                          ),
                        ],
                      ),
                      if (_filterByLocation) ...[
                        const SizedBox(height: 16),
                        Row(
                          children: [
                            Expanded(
                              child: TextField(
                                controller: _latController,
                                decoration: const InputDecoration(
                                  labelText: 'Latitude (-90 to 90)',
                                  border: OutlineInputBorder(),
                                  hintText: 'e.g. 51.5',
                                ),
                                keyboardType: const TextInputType.numberWithOptions(decimal: true),
                              ),
                            ),
                            const SizedBox(width: 12),
                            Expanded(
                              child: TextField(
                                controller: _lonController,
                                decoration: const InputDecoration(
                                  labelText: 'Longitude (-180 to 180)',
                                  border: OutlineInputBorder(),
                                  hintText: 'e.g. -0.1',
                                ),
                                keyboardType: const TextInputType.numberWithOptions(decimal: true),
                              ),
                            ),
                          ],
                        ),
                      ],
                    ],
                  ),
                ),
              ),

              const SizedBox(height: 24),
              Row(
                mainAxisSize: MainAxisSize.min,
                children: [
                  const Text('Modern Logic'),
                  Switch(
                    value: _useLegacy,
                    onChanged: (val) => setState(() => _useLegacy = val),
                    activeThumbColor: Colors.orange,
                  ),
                  const Text('Legacy (1980)'),
                ],
              ),
              const SizedBox(height: 32),
              ElevatedButton.icon(
                onPressed: _calculate,
                icon: const Icon(Icons.search),
                label: const Text('Calculate Eclipses'),
                style: ElevatedButton.styleFrom(
                  padding: const EdgeInsets.symmetric(horizontal: 32, vertical: 16),
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }
}
