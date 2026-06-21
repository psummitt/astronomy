import 'package:flutter/material.dart';

void main() {
  runApp(const JulianApp());
}

class JulianApp extends StatelessWidget {
  const JulianApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Julian Day Calculator',
      theme: ThemeData(
        primarySwatch: Colors.blue,
        useMaterial3: true,
      ),
      home: const JulianHomePage(),
    );
  }
}

class JulianHomePage extends StatefulWidget {
  const JulianHomePage({super.key});

  @override
  State<JulianHomePage> createState() => _JulianHomePageState();
}

class _JulianHomePageState extends State<JulianHomePage> {
  final TextEditingController _yearController = TextEditingController();
  final TextEditingController _monthController = TextEditingController();
  final TextEditingController _dayController = TextEditingController();
  String _result = '';

  void _calculateJD() {
    int? y1 = int.tryParse(_yearController.text);
    int? m1 = int.tryParse(_monthController.text);
    int? d1 = int.tryParse(_dayController.text);

    if (y1 == null || m1 == null || d1 == null) {
      setState(() {
        _result = 'Please enter valid numbers';
      });
      return;
    }

    // Logic from Julian.bas
    double workingY1 = y1.toDouble();
    if (workingY1 < 0) {
      workingY1 = workingY1 - 100;
    }
    
    double y = workingY1 - 1900;
    int m2 = m1 - 1;
    int dy = 0;

    if (m2 == 1) dy = 31;
    else if (m2 == 2) dy = 59;
    else if (m2 == 3) dy = 90;
    else if (m2 == 4) dy = 120;
    else if (m2 == 5) dy = 151;
    else if (m2 == 6) dy = 181;
    else if (m2 == 7) dy = 212;
    else if (m2 == 8) dy = 243;
    else if (m2 == 9) dy = 273;
    else if (m2 == 10) dy = 304;
    else if (m2 == 11) dy = 334;

    int d3 = d1 + dy;
    double d4 = 365.0 * y + (y / 4).floor();

    if (workingY1 > 1999) {
      d4 = d4 - 2;
    }

    double d5 = 15020.0 + d4.floor() + d3;
    double jd = d5;

    if (y / 4 - (y / 4).floor() == 0 && m1 < 3) {
      jd = jd - 1.0;
    }

    jd = jd + 2400000.0;
    jd = jd - ((workingY1 - 1900) / 100).floor().toDouble();

    if (workingY1 < 1583) {
      jd = jd + (10 - ((1583 - workingY1) / 100).floor()).toDouble();
    }

    if (workingY1 > 1999) {
      jd = jd + 3.0;
    }

    setState(() {
      _result = 'JULIAN DAY IS ${jd.toInt()}';
    });
  }

  void _clearFields() {
    _yearController.clear();
    _monthController.clear();
    _dayController.clear();
    setState(() {
      _result = '';
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Julian Day Calculator'),
        backgroundColor: Theme.of(context).colorScheme.primaryContainer,
      ),
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.stretch,
          children: [
            const Text(
              '---------------------------\n'
              '|      JULIAN DAY         |\n'
              '---------------------------',
              textAlign: TextAlign.center,
              style: TextStyle(fontFamily: 'Courier', fontWeight: FontWeight.bold),
            ),
            const SizedBox(height: 20),
            const Text(
              'AN ASTRONOMY PROGRAM\nBY ERIC BURGESS F.RBA.Sn',
              textAlign: TextAlign.center,
            ),
            const SizedBox(height: 20),
            const Text(
              'THIS PROGRAM PROVIDES THE JULIAN DAY\nFOR A CALENDAR DATE 1100 TO 2200',
              textAlign: TextAlign.center,
            ),
            const SizedBox(height: 30),
            TextField(
              controller: _yearController,
              decoration: const InputDecoration(labelText: 'YEAR'),
              keyboardType: TextInputType.number,
            ),
            TextField(
              controller: _monthController,
              decoration: const InputDecoration(labelText: 'MONTH'),
              keyboardType: TextInputType.number,
            ),
            TextField(
              controller: _dayController,
              decoration: const InputDecoration(labelText: 'DAY'),
              keyboardType: TextInputType.number,
            ),
            const SizedBox(height: 20),
            ElevatedButton(
              onPressed: _calculateJD,
              child: const Text('CALCULATE'),
            ),
            const SizedBox(height: 10),
            ElevatedButton(
              onPressed: _clearFields,
              child: const Text('CLEAR'),
            ),
            const SizedBox(height: 30),
            Text(
              _result,
              textAlign: TextAlign.center,
              style: const TextStyle(fontSize: 20, fontWeight: FontWeight.bold),
            ),
          ],
        ),
      ),
    );
  }
}
