import 'package:flutter/material.dart';

class HistoryPage extends StatelessWidget {
  const HistoryPage({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('History')),
      body: const Padding(
        padding: EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'Application History',
              style: TextStyle(fontSize: 22, fontWeight: FontWeight.bold),
            ),
            SizedBox(height: 16),
            Text(
              'This application is a modern Flutter port of the original SEARCH EPHEMERIDES program that was published in the October 1983 issue of 80 Micro magazine.',
            ),
            SizedBox(height: 16),
            Text(
              'COPYRIGHT 1982 BY J.H. FOX, APTON MN',
              style: TextStyle(fontWeight: FontWeight.bold),
            ),
            SizedBox(height: 16),
            Text(
              'The original logic was implemented in BASIC for early home computers and supported only single precision arithmetic. It supports Parabolic, Elliptical, and Nearly Parabolic orbits using Gaussian constants and traditional orbital mechanics formulas.',
            ),
            SizedBox(height: 16),
            Text(
              'Ported to Dart/Flutter in 2026 for cross-platform availability on Android, Web, and Windows. Double precision arithmetic was added.',
            ),
          ],
        ),
      ),
    );
  }
}
