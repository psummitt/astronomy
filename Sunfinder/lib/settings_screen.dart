import 'package:flutter/material.dart';
import 'settings_model.dart';
import 'main.dart';

class SettingsScreen extends StatefulWidget {
  final SunfinderSettings settings;
  const SettingsScreen({super.key, required this.settings});

  @override
  State<SettingsScreen> createState() => _SettingsScreenState();
}

class _SettingsScreenState extends State<SettingsScreen> {
  final _formKey = GlobalKey<FormState>();
  late double _latitude;
  late double _longitude;
  late double _meridian;
  late double _variation;
  late bool _isLeap;

  @override
  void initState() {
    super.initState();
    _latitude = widget.settings.latitude;
    _longitude = widget.settings.longitude;
    _meridian = widget.settings.standardMeridian;
    _variation = widget.settings.magneticVariation;
    _isLeap = widget.settings.isLeapYear;
  }

  void _save() {
    if (_formKey.currentState!.validate()) {
      _formKey.currentState!.save();
      final newSettings = SunfinderSettings(
        latitude: _latitude,
        longitude: _longitude,
        standardMeridian: _meridian,
        magneticVariation: _variation,
        isLeapYear: _isLeap,
        isModernMode: widget.settings.isModernMode,
      );
      SunfinderApp.of(context).updateSettings(newSettings);
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(content: Text('Settings saved successfully')),
      );
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Solar Settings')),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(24),
        child: Center(
          child: ConstrainedBox(
            constraints: const BoxConstraints(maxWidth: 500),
            child: Form(
              key: _formKey,
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.stretch,
                children: [
                  _buildSectionHeader('Location'),
                  _buildTextField(
                    label: 'Latitude',
                    hint: 'XX.X degrees (- if South)',
                    initialValue: _latitude.toString(),
                    onSaved: (val) => _latitude = double.tryParse(val!) ?? 0.0,
                  ),
                  _buildTextField(
                    label: 'Longitude',
                    hint: 'XX.X degrees (- if East)',
                    initialValue: _longitude.toString(),
                    onSaved: (val) => _longitude = double.tryParse(val!) ?? 0.0,
                  ),
                  const SizedBox(height: 24),
                  _buildSectionHeader('Solar Data'),
                  _buildTextField(
                    label: 'Standard Meridian',
                    hint: 'Standard Meridian degrees',
                    initialValue: _meridian.toString(),
                    onSaved: (val) => _meridian = double.tryParse(val!) ?? 0.0,
                  ),
                  _buildTextField(
                    label: 'Magnetic Variation',
                    hint: 'XX.X degrees (- if East)',
                    initialValue: _variation.toString(),
                    onSaved: (val) => _variation = double.tryParse(val!) ?? 0.0,
                  ),
                  const SizedBox(height: 16),
                  SwitchListTile(
                    title: const Text('Leap Year'),
                    subtitle: const Text('Adjust calculations for 366 days'),
                    value: _isLeap,
                    onChanged: (val) => setState(() => _isLeap = val),
                  ),
                  const SizedBox(height: 32),
                  ElevatedButton(
                    onPressed: _save,
                    style: ElevatedButton.styleFrom(
                      padding: const EdgeInsets.symmetric(vertical: 16),
                    ),
                    child: const Text('Save Settings', style: TextStyle(fontSize: 16)),
                  ),
                ],
              ),
            ),
          ),
        ),
      ),
    );
  }

  Widget _buildSectionHeader(String title) {
    return Padding(
      padding: const EdgeInsets.only(bottom: 12),
      child: Text(
        title,
        style: const TextStyle(fontWeight: FontWeight.bold, fontSize: 18, color: Colors.amber),
      ),
    );
  }

  Widget _buildTextField({
    required String label,
    required String hint,
    required String initialValue,
    required FormFieldSetter<String> onSaved,
  }) {
    return Padding(
      padding: const EdgeInsets.only(bottom: 16),
      child: TextFormField(
        initialValue: initialValue,
        decoration: InputDecoration(
          labelText: label,
          hintText: hint,
          border: const OutlineInputBorder(),
        ),
        keyboardType: const TextInputType.numberWithOptions(decimal: true, signed: true),
        onSaved: onSaved,
        validator: (value) => value == null || double.tryParse(value) == null ? 'Enter a valid number' : null,
      ),
    );
  }
}
