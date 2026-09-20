import 'package:flutter/material.dart';

class HistoryScreen extends StatelessWidget {
  const HistoryScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      padding: const EdgeInsets.all(24.0),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            'The Legacy of RADEC',
            style: Theme.of(context).textTheme.headlineMedium,
          ),
          const SizedBox(height: 16),
          const Text(
            'The original "RADEC" program was written in Applesoft BASIC by Eric Burgess, F.R.A.S. (Fellow of the Royal Astronomical Society). Eric Burgess was a pioneer in space journalism, one of the founders of the British Interplanetary Society, and a man who famously helped convince NASA to include the "Pioneer Plaque" on the Pioneer 10 and 11 spacecraft.',
          ),
          const SizedBox(height: 16),
          const Text(
            'Originally published and distributed by S & T Software Service in the late 1970s, the program was designed to bring complex astronomical calculations to the early home computer revolution. It provided amateur astronomers with the ability to calculate the right ascension, declination, and distances of all planets directly on their Apple II computers, long before the internet made such data ubiquitous.',
          ),
          const SizedBox(height: 24),
          Text(
            'Mathematical Foundations',
            style: Theme.of(context).textTheme.titleLarge,
          ),
          const SizedBox(height: 8),
          const Text(
            'The core logic of the original program is centered around calculations from the Epoch of 1960.0. It uses mean motion, eccentricity, and heliocentric longitudes to derive the positions of the planets. This specific mathematical approach was a hallmark of late 20th-century orbital mechanics designed for the limited memory and processing power of 8-bit systems like the MOS 6502.',
          ),
          const SizedBox(height: 24),
          Text(
            'From BASIC to Multi-Platform Flutter',
            style: Theme.of(context).textTheme.titleLarge,
          ),
          const SizedBox(height: 8),
          const Text(
            'This modern incarnation, built in 2026, preserves the original 1970s logic (available as the "Local" mode) while porting it to the Dart programming language. By leveraging the Flutter framework, the program has transitioned from a single-machine BASIC file to a modern, responsive application running on Android, Linux, Web, and Windows.',
          ),
          const SizedBox(height: 24),
          Text(
            'Bridging the Centuries',
            style: Theme.of(context).textTheme.titleLarge,
          ),
          const SizedBox(height: 8),
          const Text(
            'To complement the historical logic, this app introduces a "Remote" mode that connects to NASA\'s Jet Propulsion Laboratory (JPL) Horizons system. This bridges the gap between the approximations of the 1970s and the high-precision ephemeris data used by modern space agencies, allowing users to compare the heritage of computing with the cutting edge of science.',
          ),
          const SizedBox(height: 32),
          const Divider(),
          const SizedBox(height: 16),
          const Center(
            child: Text(
              'Dedicated to Eric Burgess and the pioneers of early astronomical computing.',
              textAlign: TextAlign.center,
              style: TextStyle(fontStyle: FontStyle.italic, color: Colors.grey),
            ),
          ),
          const SizedBox(height: 32),
        ],
      ),
    );
  }
}
