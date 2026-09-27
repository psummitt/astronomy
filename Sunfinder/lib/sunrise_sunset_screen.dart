import 'package:flutter/material.dart';
import 'settings_model.dart';
import 'solar_calculator.dart';

class SunriseSunsetScreen extends StatefulWidget {
  final SunfinderSettings settings;
  const SunriseSunsetScreen({super.key, required this.settings});

  @override
  State<SunriseSunsetScreen> createState() => _SunriseSunsetScreenState();
}

class _SunriseSunsetScreenState extends State<SunriseSunsetScreen> {
  final _formKey = GlobalKey<FormState>();
  int _startMonth = 1, _startDay = 1;
  int _endMonth = 1, _endDay = 1;
  int _dayIncrement = 1;
  List<SunriseSunsetResult> _results = [];

  void _calculate() {
    if (_formKey.currentState!.validate()) {
      _formKey.currentState!.save();
      final startDoy = SolarCalculator.getDayOfYear(_startMonth, _startDay, widget.settings.isLeapYear);
      final endDoy = SolarCalculator.getDayOfYear(_endMonth, _endDay, widget.settings.isLeapYear);
      
      setState(() {
        _results = SolarCalculator.calculateSunriseSunset(
          latitude: widget.settings.latitude,
          longitude: widget.settings.longitude,
          standardMeridian: widget.settings.standardMeridian,
          magneticVariation: widget.settings.magneticVariation,
          isLeapYear: widget.settings.isLeapYear,
          startDoy: startDoy,
          endDoy: endDoy,
          dayIncrement: _dayIncrement,
        );
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Sunrise & Sunset")),
      body: Column(
        children: [
          Padding(
            padding: const EdgeInsets.all(16.0),
            child: Form(
              key: _formKey,
              child: Column(
                children: [
                  Row(
                    children: [
                      Expanded(child: _buildNumberField("Start Month", (v) => _startMonth = int.parse(v!))),
                      const SizedBox(width: 8),
                      Expanded(child: _buildNumberField("Start Day", (v) => _startDay = int.parse(v!))),
                    ],
                  ),
                  const SizedBox(height: 8),
                  Row(
                    children: [
                      Expanded(child: _buildNumberField("End Month", (v) => _endMonth = int.parse(v!))),
                      const SizedBox(width: 8),
                      Expanded(child: _buildNumberField("End Day", (v) => _endDay = int.parse(v!))),
                      const SizedBox(width: 8),
                      Expanded(child: _buildNumberField("Day Step", (v) => _dayIncrement = int.parse(v!))),
                    ],
                  ),
                  const SizedBox(height: 16),
                  ElevatedButton(onPressed: _calculate, child: const Text("Calculate Sunrise/Sunset")),
                ],
              ),
            ),
          ),
          Expanded(
            child: _results.isEmpty 
              ? const Center(child: Text("Enter dates and click calculate"))
              : ListView.builder(
                  itemCount: _results.length,
                  itemBuilder: (context, index) {
                    final r = _results[index];
                    return Card(
                      margin: const EdgeInsets.symmetric(horizontal: 16, vertical: 4),
                      child: Padding(
                        padding: const EdgeInsets.all(12.0),
                        child: Column(
                          crossAxisAlignment: CrossAxisAlignment.start,
                          children: [
                            Text(r.dateString, style: const TextStyle(fontWeight: FontWeight.bold, fontSize: 16)),
                            const Divider(),
                            Row(
                              mainAxisAlignment: MainAxisAlignment.spaceBetween,
                              children: [
                                Column(
                                  crossAxisAlignment: CrossAxisAlignment.start,
                                  children: [
                                    const Text("Sunrise", style: TextStyle(color: Colors.orange)),
                                    Text("${r.sunrise} @ ${r.sunriseAzimuth}°"),
                                  ],
                                ),
                                Column(
                                  crossAxisAlignment: CrossAxisAlignment.start,
                                  children: [
                                    const Text("Sunset", style: TextStyle(color: Colors.deepOrange)),
                                    Text("${r.sunset} @ ${r.sunsetAzimuth}°"),
                                  ],
                                ),
                                Column(
                                  crossAxisAlignment: CrossAxisAlignment.start,
                                  children: [
                                    const Text("Noon", style: TextStyle(color: Colors.amber)),
                                    Text("Alt: ${r.noonAltitude}° | Az: ${r.noonAzimuth}°"),
                                  ],
                                ),
                              ],
                            ),
                          ],
                        ),
                      ),
                    );
                  },
                ),
          ),
        ],
      ),
    );
  }

  Widget _buildNumberField(String label, FormFieldSetter<String> onSaved) {
    return TextFormField(
      decoration: InputDecoration(labelText: label, border: const OutlineInputBorder()),
      keyboardType: TextInputType.number,
      onSaved: onSaved,
      validator: (v) => v == null || v.isEmpty ? "Required" : null,
    );
  }
}
