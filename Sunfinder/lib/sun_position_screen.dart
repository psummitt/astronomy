import 'package:flutter/material.dart';
import 'settings_model.dart';
import 'solar_calculator.dart';

class SunPositionScreen extends StatefulWidget {
  final SunfinderSettings settings;
  const SunPositionScreen({super.key, required this.settings});

  @override
  State<SunPositionScreen> createState() => _SunPositionScreenState();
}

class _SunPositionScreenState extends State<SunPositionScreen> {
  final _formKey = GlobalKey<FormState>();
  int _startMonth = 1, _startDay = 1;
  int _endMonth = 1, _endDay = 1;
  double _startHour = 0, _endHour = 24, _increment = 1;
  List<SolarResult> _results = [];

  void _calculate() {
    if (_formKey.currentState!.validate()) {
      _formKey.currentState!.save();
      final startDoy = SolarCalculator.getDayOfYear(_startMonth, _startDay, widget.settings.isLeapYear);
      final endDoy = SolarCalculator.getDayOfYear(_endMonth, _endDay, widget.settings.isLeapYear);
      
      setState(() {
        _results = SolarCalculator.calculatePosition(
          latitude: widget.settings.latitude,
          longitude: widget.settings.longitude,
          standardMeridian: widget.settings.standardMeridian,
          magneticVariation: widget.settings.magneticVariation,
          isLeapYear: widget.settings.isLeapYear,
          startDoy: startDoy,
          endDoy: endDoy,
          startHour: _startHour,
          endHour: _endHour,
          hourIncrement: _increment,
        );
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Where's the Sun")),
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
                      Expanded(child: _buildNumberField("Start Month (1-12)", (v) => _startMonth = int.parse(v!))),
                      const SizedBox(width: 8),
                      Expanded(child: _buildNumberField("Start Day (1-31)", (v) => _startDay = int.parse(v!))),
                    ],
                  ),
                  const SizedBox(height: 8),
                  Row(
                    children: [
                      Expanded(child: _buildNumberField("End Month (1-12)", (v) => _endMonth = int.parse(v!))),
                      const SizedBox(width: 8),
                      Expanded(child: _buildNumberField("End Day (1-31)", (v) => _endDay = int.parse(v!))),
                    ],
                  ),
                  const SizedBox(height: 8),
                  Row(
                    children: [
                      Expanded(child: _buildNumberField("Start Hour (0-24)", (v) => _startHour = double.parse(v!))),
                      const SizedBox(width: 8),
                      Expanded(child: _buildNumberField("End Hour (0-24)", (v) => _endHour = double.parse(v!))),
                      const SizedBox(width: 8),
                      Expanded(child: _buildNumberField("Step (Hours)", (v) => _increment = double.parse(v!))),
                    ],
                  ),
                  const SizedBox(height: 16),
                  ElevatedButton(onPressed: _calculate, child: const Text("Calculate Position")),
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
                      child: ListTile(
                        title: Text("${r.dateString} at ${r.hour.toStringAsFixed(1)}h"),
                        subtitle: Text("Azimuth: ${r.azimuth}° | Altitude: ${r.altitude}°"),
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
