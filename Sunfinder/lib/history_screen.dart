import 'package:flutter/material.dart';

class HistoryScreen extends StatelessWidget {
  const HistoryScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Program History')),
      body: const SingleChildScrollView(
        padding: EdgeInsets.all(24),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text('The Sunfinder Legacy', style: TextStyle(fontSize: 24, fontWeight: FontWeight.bold)),
            SizedBox(height: 16),
            Text(
              'Sunfinder was originally created by Harris Smith for the Radio Shack TRS-80 microcomputer. It was first published in the October 1983 issue of 80 Micro magazine in an article titled "Catching Rays."',
              style: TextStyle(fontSize: 16, height: 1.5),
            ),
            SizedBox(height: 16),
            Text(
              'Designed for researchers and navigators, the program allowed users to track the solar path with high precision using spherical trigonometry. A technical correction followed in the March 1984 issue, refining the handling of magnetic deviation.',
              style: TextStyle(fontSize: 16, height: 1.5),
            ),
            SizedBox(height: 16),
            Text(
              'This modern Flutter conversion preserves the original mathematical logic while introducing a responsive interface accessible on all modern devices. It serves as a bridge between 1980s hobbyist computing and today\'s cross-platform software standards.',
              style: TextStyle(fontSize: 16, height: 1.5),
            ),
            SizedBox(height: 32),
            Center(
              child: Icon(Icons.computer, size: 60, color: Colors.grey),
            ),
            SizedBox(height: 8),
            Center(
              child: Text('TRS-80 Model I/III/4 (1983)', style: TextStyle(color: Colors.grey, fontStyle: FontStyle.italic)),
            ),
          ],
        ),
      ),
    );
  }
}
