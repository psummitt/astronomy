import 'dart:math' as math;
import 'package:flutter/material.dart';
import 'package:intl/intl.dart';
import '../logic/base_calculator.dart';
import '../logic/eclipse_event.dart';

class ResultsScreen extends StatelessWidget {
  final int startYear;
  final BaseCalculator calculator;
  final double? userLat;
  final double? userLon;

  const ResultsScreen({
    super.key,
    required this.startYear,
    required this.calculator,
    this.userLat,
    this.userLon,
  });

  bool isVisible(double? moonLat, double? moonLon) {
    if (userLat == null || userLon == null || moonLat == null || moonLon == null) {
      return true; // No filter applied
    }

    double rad(double deg) => deg * math.pi / 180;

    double phi1 = rad(userLat!);
    double phi2 = rad(moonLat);
    double deltaLambda = rad(moonLon - userLon!);

    // Spherical law of cosines for angular distance
    double cosTheta = math.sin(phi1) * math.sin(phi2) + 
                      math.cos(phi1) * math.cos(phi2) * math.cos(deltaLambda);
    
    // Above horizon if cosTheta > 0 (distance < 90 degrees)
    return cosTheta > 0;
  }

  @override
  Widget build(BuildContext context) {
    final allEvents = calculator.calculateEclipses(startYear, count: 20);
    final dateFormat = DateFormat('EEEE, MMMM d, yyyy');
    final timeFormat = DateFormat('HH:mm');
    final now = DateTime.now().toUtc();

    // Filter results based on visibility if coordinates provided
    final events = allEvents.where((e) => isVisible(e.latitude, e.longitude)).toList();

    return Scaffold(
      appBar: AppBar(
        title: const Text('Lunar Eclipse Calculator'),
      ),
      body: events.isEmpty
          ? Center(
              child: Padding(
                padding: const EdgeInsets.all(24.0),
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    const Icon(Icons.location_off, size: 64, color: Colors.grey),
                    const SizedBox(height: 16),
                    const Text(
                      'No eclipses visible from this location in the selected year.',
                      textAlign: TextAlign.center,
                      style: TextStyle(fontSize: 18, color: Colors.grey),
                    ),
                    const SizedBox(height: 8),
                    TextButton(
                      onPressed: () => Navigator.pop(context),
                      child: const Text('Change Criteria'),
                    ),
                  ],
                ),
              ),
            )
          : ListView.separated(
              itemCount: events.length,
              padding: const EdgeInsets.all(16),
              separatorBuilder: (context, index) {
                final currentEvent = events[index];
                final nextEvent = index + 1 < events.length ? events[index + 1] : null;
                
                if (nextEvent != null && 
                    currentEvent.date.isBefore(now) && 
                    nextEvent.date.isAfter(now)) {
                  return const Padding(
                    padding: EdgeInsets.symmetric(vertical: 8.0),
                    child: Row(
                      children: [
                        Expanded(child: Divider()),
                        Padding(
                          padding: EdgeInsets.symmetric(horizontal: 8.0),
                          child: Text('UPCOMING', style: TextStyle(fontWeight: FontWeight.bold, color: Colors.indigo)),
                        ),
                        Expanded(child: Divider()),
                      ],
                    ),
                  );
                }
                return const SizedBox(height: 16);
              },
              itemBuilder: (context, index) {
                final event = events[index];
                final isPast = event.date.toUtc().isBefore(now);
                
                return Opacity(
                  opacity: isPast ? 0.65 : 1.0,
                  child: Card(
                    margin: EdgeInsets.zero,
                    elevation: isPast ? 0 : 2,
                    child: ListTile(
                      leading: CircleAvatar(
                        backgroundColor: isPast ? Colors.grey[200] : null,
                        child: Icon(Icons.brightness_2, color: isPast ? Colors.grey[500] : null),
                      ),
                      title: Text(
                        event.typeName,
                        style: TextStyle(
                          fontWeight: FontWeight.bold,
                          color: isPast ? Colors.grey[700] : Colors.indigo,
                        ),
                      ),
                      subtitle: Column(
                        crossAxisAlignment: CrossAxisAlignment.start,
                        children: [
                          Text(
                            dateFormat.format(event.date.toLocal()),
                            style: const TextStyle(fontWeight: FontWeight.w500),
                          ),
                          Text(
                            'Time: ${timeFormat.format(event.date.toLocal())} (Local)',
                            style: TextStyle(color: isPast ? Colors.grey[600] : null),
                          ),
                          if (event.type == EclipseType.penumbral)
                            Text(
                              'Penumbral Magnitude: ${event.penumbralMagnitude?.toStringAsFixed(3)}',
                              style: TextStyle(color: isPast ? Colors.grey[600] : null),
                            )
                          else
                            Text(
                              'Umbral Magnitude: ${event.magnitude.toStringAsFixed(3)}',
                              style: TextStyle(color: isPast ? Colors.grey[600] : null),
                            ),
                          if (event.bestLocation != null)
                            Text(
                              'Best View: ${event.bestLocation}',
                              style: TextStyle(color: isPast ? Colors.grey[600] : null),
                            ),
                          if (isPast)
                            Padding(
                              padding: const EdgeInsets.only(top: 4.0),
                              child: Text(
                                '(PAST EVENT)',
                                style: TextStyle(
                                  fontStyle: FontStyle.italic,
                                  color: Colors.grey[600],
                                  fontSize: 10,
                                  letterSpacing: 1.1,
                                ),
                              ),
                            ),
                        ],
                      ),
                      trailing: magnitudeIcon(event.magnitude, isPast: isPast),
                    ),
                  ),
                );
              },
            ),
    );
  }

  Widget magnitudeIcon(double mag, {bool isPast = false}) {
    Color? color;
    if (mag > 1.0) {
      color = isPast ? Colors.grey : Colors.deepPurple;
      return Icon(Icons.visibility, color: color, semanticLabel: 'Total Eclipse');
    } else {
      color = isPast ? Colors.grey : Colors.orange;
      return Icon(Icons.brightness_4, color: color, semanticLabel: 'Partial Eclipse');
    }
  }
}
