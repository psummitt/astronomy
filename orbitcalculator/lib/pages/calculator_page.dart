import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import '../services/orbit_provider.dart';
import '../services/orbit_engine.dart';
import '../models/solar_coordinates.dart';

class CalculatorPage extends StatelessWidget {
  const CalculatorPage({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Orbit Calculator'),
        actions: [
          Consumer<OrbitProvider>(
            builder: (context, provider, child) {
              return Row(
                children: [
                  const Text('Single'),
                  Switch(
                    value: provider.precision == Precision.double,
                    onChanged: (value) {
                      provider.setPrecision(value ? Precision.double : Precision.single);
                    },
                  ),
                  const Text('Double'),
                  const SizedBox(width: 8),
                ],
              );
            },
          ),
        ],
      ),
      body: Consumer<OrbitProvider>(
        builder: (context, provider, child) {
          return SingleChildScrollView(
            padding: const EdgeInsets.all(16.0),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.stretch,
              children: [
                _buildElementsForm(context, provider),
                const SizedBox(height: 16),
                _buildSolarCoordinatesList(context, provider),
                const SizedBox(height: 16),
                ElevatedButton(
                  onPressed: () {
                    provider.calculate();
                  },
                  child: const Padding(
                    padding: EdgeInsets.symmetric(vertical: 12.0),
                    child: Text('CALCULATE EPHEMERIS', style: TextStyle(fontSize: 18)),
                  ),
                ),
                const SizedBox(height: 16),
                _buildResults(context, provider),
              ],
            ),
          );
        },
      ),
    );
  }

  Widget _buildElementsForm(BuildContext context, OrbitProvider provider) {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(12.0),
        child: Column(
          children: [
            Text('Orbital Elements', style: Theme.of(context).textTheme.titleLarge),
            TextField(
              decoration: const InputDecoration(
                labelText: 'Name of Object',
                hintText: 'e.g. Halley\'s Comet',
              ),
              onChanged: (v) => provider.elements.name = v,
            ),
            Row(
              children: [
                Expanded(
                  child: Semantics(
                    label: 'Inclination in degrees',
                    child: TextField(
                      decoration: const InputDecoration(labelText: 'Inclination (deg)'),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true),
                      onChanged: (v) => provider.elements.inclination = double.tryParse(v) ?? 0,
                    ),
                  ),
                ),
                const SizedBox(width: 8),
                Expanded(
                  child: Semantics(
                    label: 'Argument of Perihelion in degrees',
                    child: TextField(
                      decoration: const InputDecoration(labelText: 'Arg. of Perihelion (deg)'),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true),
                      onChanged: (v) => provider.elements.argumentOfPerihelion = double.tryParse(v) ?? 0,
                    ),
                  ),
                ),
              ],
            ),
            Row(
              children: [
                Expanded(
                  child: Semantics(
                    label: 'Longitude of Ascending Node in degrees',
                    child: TextField(
                      decoration: const InputDecoration(labelText: 'Long. Asc. Node (deg)'),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true),
                      onChanged: (v) => provider.elements.longitudeOfAscendingNode = double.tryParse(v) ?? 0,
                    ),
                  ),
                ),
                const SizedBox(width: 8),
                Expanded(
                  child: Semantics(
                    label: 'Reference Date in Julian Date',
                    child: TextField(
                      decoration: const InputDecoration(labelText: 'Ref. Date (JD)'),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true),
                      onChanged: (v) => provider.elements.referenceDate = double.tryParse(v) ?? 0,
                    ),
                  ),
                ),
              ],
            ),
            Row(
              children: [
                Expanded(
                  child: Semantics(
                    label: 'Eccentricity',
                    child: TextField(
                      decoration: const InputDecoration(labelText: 'Eccentricity'),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true),
                      onChanged: (v) => provider.elements.eccentricity = double.tryParse(v) ?? 0,
                    ),
                  ),
                ),
                const SizedBox(width: 8),
                Expanded(
                  child: Semantics(
                    label: 'A value, Perihelion distance or Semi-major axis',
                    child: TextField(
                      decoration: const InputDecoration(labelText: 'A (Peri. Dist / Semi-Maj)'),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true),
                      onChanged: (v) => provider.elements.a = double.tryParse(v) ?? 0,
                    ),
                  ),
                ),
              ],
            ),
            Semantics(
              label: 'Mean Anomaly in degrees',
              child: TextField(
                decoration: const InputDecoration(labelText: 'Mean Anomaly (deg)'),
                keyboardType: const TextInputType.numberWithOptions(decimal: true),
                onChanged: (v) => provider.elements.meanAnomaly = double.tryParse(v) ?? 0,
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildSolarCoordinatesList(BuildContext context, OrbitProvider provider) {
    return ExpansionTile(
      title: const Text('Solar Coordinates (up to 10 dates)'),
      children: List.generate(10, (index) {
        return Padding(
          padding: const EdgeInsets.symmetric(horizontal: 16.0, vertical: 4.0),
          child: Column(
            children: [
              Text('Date ${index + 1}', style: const TextStyle(fontWeight: FontWeight.bold)),
              Row(
                children: [
                  Expanded(
                    flex: 2,
                    child: Semantics(
                      label: 'Date for coordinate $index',
                      child: TextField(
                        decoration: const InputDecoration(labelText: 'Date (e.g. 10/15/82)'),
                        onChanged: (v) => provider.solarCoordinatesList[index].date = v,
                      ),
                    ),
                  ),
                  const SizedBox(width: 8),
                  Expanded(
                    flex: 3,
                    child: Semantics(
                      label: 'Julian Date for coordinate $index',
                      child: TextField(
                        decoration: const InputDecoration(labelText: 'JD'),
                        keyboardType: const TextInputType.numberWithOptions(decimal: true),
                        onChanged: (v) => provider.solarCoordinatesList[index].julianDate = double.tryParse(v) ?? 0,
                      ),
                    ),
                  ),
                ],
              ),
              Row(
                children: [
                  Expanded(
                    child: Semantics(
                      label: 'X solar coordinate for $index',
                      child: TextField(
                        decoration: const InputDecoration(labelText: 'X'),
                        keyboardType: const TextInputType.numberWithOptions(decimal: true),
                        onChanged: (v) => provider.solarCoordinatesList[index].x = double.tryParse(v) ?? 0,
                      ),
                    ),
                  ),
                  const SizedBox(width: 8),
                  Expanded(
                    child: Semantics(
                      label: 'Y solar coordinate for $index',
                      child: TextField(
                        decoration: const InputDecoration(labelText: 'Y'),
                        keyboardType: const TextInputType.numberWithOptions(decimal: true),
                        onChanged: (v) => provider.solarCoordinatesList[index].y = double.tryParse(v) ?? 0,
                      ),
                    ),
                  ),
                  const SizedBox(width: 8),
                  Expanded(
                    child: Semantics(
                      label: 'Z solar coordinate for $index',
                      child: TextField(
                        decoration: const InputDecoration(labelText: 'Z'),
                        keyboardType: const TextInputType.numberWithOptions(decimal: true),
                        onChanged: (v) => provider.solarCoordinatesList[index].z = double.tryParse(v) ?? 0,
                      ),
                    ),
                  ),
                ],
              ),
              const Divider(),
            ],
          ),
        );
      }),
    );
  }

  Widget _buildResults(BuildContext context, OrbitProvider provider) {
    if (provider.results.isEmpty) return const SizedBox.shrink();

    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text('Results for ${provider.elements.name}', style: Theme.of(context).textTheme.titleLarge),
        const SizedBox(height: 8),
        ListView.builder(
          shrinkWrap: true,
          physics: const NeverScrollableScrollPhysics(),
          itemCount: provider.results.length,
          itemBuilder: (context, index) {
            final res = provider.results[index];
            return Semantics(
              label: 'Calculation result for ${res.date}',
              child: Card(
                child: ListTile(
                  title: Text('Date: ${res.date}'),
                  subtitle: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Text('RA: ${res.raHours}h ${res.raMinutes.toStringAsFixed(1)}m'),
                      Text('Decl: ${res.decDegrees}° ${res.decMinutes.toStringAsFixed(1)}\''),
                      Text('Dist (Earth): ${res.distanceEarth.toStringAsFixed(4)} AU'),
                      Text('Dist (Sun): ${res.distanceSun.toStringAsFixed(4)} AU'),
                    ],
                  ),
                ),
              ),
            );
          },
        ),
      ],
    );
  }
}
