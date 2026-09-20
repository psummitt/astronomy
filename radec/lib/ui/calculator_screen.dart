import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:intl/intl.dart';
import '../services/planet_provider.dart';

class CalculatorScreen extends StatelessWidget {
  const CalculatorScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return Consumer<PlanetProvider>(
      builder: (context, provider, child) {
        return Padding(
          padding: const EdgeInsets.all(16.0),
          child: Column(
            children: [
              _buildControls(context, provider),
              const SizedBox(height: 16),
              Expanded(
                child: provider.isLoading
                    ? const Center(child: CircularProgressIndicator())
                    : provider.errorMessage != null
                        ? Center(
                            child: Padding(
                              padding: const EdgeInsets.all(20.0),
                              child: Text(
                                provider.errorMessage!,
                                textAlign: TextAlign.center,
                                style: const TextStyle(color: Colors.red),
                              ),
                            ),
                          )
                        : _buildResultTable(context, provider),
              ),
            ],
          ),
        );
      },
    );
  }

  Widget _buildControls(BuildContext context, PlanetProvider provider) {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(8.0),
        child: Wrap(
          spacing: 20,
          runSpacing: 10,
          alignment: WrapAlignment.center,
          crossAxisAlignment: WrapCrossAlignment.center,
          children: [
            Row(
              mainAxisSize: MainAxisSize.min,
              children: [
                const Text('Date: '),
                Semantics(
                  button: true,
                  label: 'Select date for calculation',
                  child: TextButton(
                    onPressed: () async {
                      final date = await showDatePicker(
                        context: context,
                        initialDate: provider.selectedDate,
                        firstDate: DateTime(1800),
                        lastDate: DateTime(2100),
                      );
                      if (date != null) provider.setDate(date);
                    },
                    child: Text(DateFormat('yyyy-MM-dd').format(provider.selectedDate)),
                  ),
                ),
              ],
            ),
            Row(
              mainAxisSize: MainAxisSize.min,
              children: [
                const Text('Source: '),
                Semantics(
                  label: 'Calculation method',
                  child: SegmentedButton<CalculationMode>(
                    segments: const [
                      ButtonSegment(
                        value: CalculationMode.local,
                        label: Text('Local (BASIC)'),
                        icon: Icon(Icons.computer),
                      ),
                      ButtonSegment(
                        value: CalculationMode.remote,
                        label: Text('Internet (NASA)'),
                        icon: Icon(Icons.cloud),
                      ),
                    ],
                    selected: {provider.mode},
                    onSelectionChanged: (set) => provider.setMode(set.first),
                  ),
                ),
              ],
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildResultTable(BuildContext context, PlanetProvider provider) {
    if (provider.planets.isEmpty) {
      return const Center(child: Text('No data available. Check internet connection for Remote mode.'));
    }

    return SingleChildScrollView(
      scrollDirection: Axis.vertical,
      child: SingleChildScrollView(
        scrollDirection: Axis.horizontal,
        child: Semantics(
          label: 'Planetary Data Table',
          child: DataTable(
            columns: const [
              DataColumn(label: Text('Planet')),
              DataColumn(label: Text('RA (h)')),
              DataColumn(label: Text('Dec (°)')),
              DataColumn(label: Text('Sun (AU)')),
              DataColumn(label: Text('Earth (AU)')),
            ],
            rows: provider.planets.map((planet) {
              return DataRow(cells: [
                DataCell(Text(planet.name)),
                DataCell(Text(planet.rightAscension.toStringAsFixed(2))),
                DataCell(Text(planet.declination.toStringAsFixed(2))),
                DataCell(Text(planet.distanceToSun.toStringAsFixed(2))),
                DataCell(Text(planet.distanceToEarth.toStringAsFixed(2))),
              ]);
            }).toList(),
          ),
        ),
      ),
    );
  }
}
