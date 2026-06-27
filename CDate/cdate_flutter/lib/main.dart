import 'package:flutter/material.dart';

void main() {
  runApp(const CDateApp());
}

class CDateApp extends StatelessWidget {
  const CDateApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Julian Day Converter',
      theme: ThemeData(
        brightness: Brightness.dark,
        primarySwatch: Colors.indigo,
        useMaterial3: true,
      ),
      home: const HomePage(),
    );
  }
}

class HomePage extends StatefulWidget {
  const HomePage({super.key});

  @override
  State<HomePage> createState() => _HomePageState();
}

class _HomePageState extends State<HomePage> {
  final TextEditingController _jdController = TextEditingController();
  final TextEditingController _yearController = TextEditingController();
  final TextEditingController _monthController = TextEditingController();
  final TextEditingController _dayController = TextEditingController();

  String _resultDate = '';
  String _resultJd = '';

  @override
  void initState() {
    super.initState();
    _jdController.addListener(_convertJdToDate);
    _yearController.addListener(_convertDateToJd);
    _monthController.addListener(_convertDateToJd);
    _dayController.addListener(_convertDateToJd);
  }

  void _convertJdToDate() {
    final jdStr = _jdController.text;
    if (jdStr.isEmpty) {
      setState(() => _resultDate = '');
      return;
    }
    final jd = double.tryParse(jdStr);
    if (jd == null) {
      setState(() => _resultDate = 'Invalid Julian Day');
      return;
    }

    final date = jdToDate(jd);
    setState(() {
      _resultDate = '${date['year']}-${date['month'].toString().padLeft(2, '0')}-${date['day'].toStringAsFixed(2).padLeft(5, '0')}';
    });
  }

  void _convertDateToJd() {
    final yStr = _yearController.text;
    final mStr = _monthController.text;
    final dStr = _dayController.text;

    if (yStr.isEmpty || mStr.isEmpty || dStr.isEmpty) {
      setState(() => _resultJd = '');
      return;
    }

    final y = int.tryParse(yStr);
    final m = int.tryParse(mStr);
    final d = double.tryParse(dStr);

    if (y == null || m == null || d == null) {
      setState(() => _resultJd = 'Invalid Date');
      return;
    }

    final jd = dateToJd(y, m, d);
    if (y < 1100 || y > 2100) {
      setState(() {
        _resultJd = '${jd.toStringAsFixed(6)} (Warning: Year $y out of 1100-2100 range)';
      });
    } else {
      setState(() {
        _resultJd = jd.toStringAsFixed(6);
      });
    }
  }

  // Algorithm from Jean Meeus, "Astronomical Algorithms"
  Map<String, dynamic> jdToDate(double jd) {
    double z = (jd + 0.5).floorToDouble();
    double f = (jd + 0.5) - z;
    double a;
    if (z < 2299161) {
      a = z;
    } else {
      double alpha = ((z - 1867216.25) / 36524.25).floorToDouble();
      a = z + 1 + alpha - (alpha / 4).floorToDouble();
    }
    double b = a + 1524;
    double c = ((b - 122.1) / 365.25).floorToDouble();
    double d = (365.25 * c).floorToDouble();
    double e = ((b - d) / 30.6001).floorToDouble();

    double day = b - d - (30.6001 * e).floorToDouble() + f;
    int month = (e < 14) ? (e - 1).toInt() : (e - 13).toInt();
    int year = (month > 2) ? (c - 4716).toInt() : (c - 4715).toInt();

    return {'year': year, 'month': month, 'day': day};
  }

  double dateToJd(int year, int month, double day) {
    int y = year;
    int m = month;
    if (m <= 2) {
      y -= 1;
      m += 12;
    }
    int a = (y / 100).floor();
    int b = 2 - a + (a / 4).floor();
    
    bool isGregorian = (year > 1582) || 
                       (year == 1582 && month > 10) || 
                       (year == 1582 && month == 10 && day >= 15);
    if (!isGregorian) b = 0;

    return (365.25 * (y + 4716)).floor() + 
           (30.6001 * (m + 1)).floor() + 
           day + b - 1524.5;
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('CDate: Julian Day Converter'),
        centerTitle: true,
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.stretch,
          children: [
            _buildHeader(),
            const SizedBox(height: 32),
            _buildJdToDateSection(),
            const Divider(height: 64),
            _buildDateToJdSection(),
            const SizedBox(height: 32),
            _buildFooter(),
          ],
        ),
      ),
    );
  }

  Widget _buildHeader() {
    return Column(
      children: [
        const Text(
          'AN ASTRONOMY PROGRAM',
          style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold, letterSpacing: 2),
        ),
        const Text('BY ERIC BURGESS F.R.A.S.'),
        const SizedBox(height: 8),
        const Text(
          'Converted to Flutter\nUpdated for 1100-2100',
          textAlign: TextAlign.center,
          style: TextStyle(fontStyle: FontStyle.italic, color: Colors.grey),
        ),
      ],
    );
  }

  Widget _buildJdToDateSection() {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            const Text('JULIAN DAY TO CALENDAR DATE', style: TextStyle(fontWeight: FontWeight.bold)),
            const SizedBox(height: 16),
            TextField(
              controller: _jdController,
              decoration: const InputDecoration(
                labelText: 'Julian Day (e.g., 2451545.0)',
                border: OutlineInputBorder(),
              ),
              keyboardType: const TextInputType.numberWithOptions(decimal: true),
            ),
            const SizedBox(height: 16),
            Text(
              'Calendar Date: $_resultDate',
              style: const TextStyle(fontSize: 20, color: Colors.lightBlueAccent),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildDateToJdSection() {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            const Text('CALENDAR DATE TO JULIAN DAY', style: TextStyle(fontWeight: FontWeight.bold)),
            const SizedBox(height: 16),
            Row(
              children: [
                Expanded(
                  flex: 2,
                  child: TextField(
                    controller: _yearController,
                    decoration: const InputDecoration(labelText: 'Year', border: OutlineInputBorder()),
                    keyboardType: TextInputType.number,
                  ),
                ),
                const SizedBox(width: 8),
                Expanded(
                  flex: 1,
                  child: TextField(
                    controller: _monthController,
                    decoration: const InputDecoration(labelText: 'Month', border: OutlineInputBorder()),
                    keyboardType: TextInputType.number,
                  ),
                ),
                const SizedBox(width: 8),
                Expanded(
                  flex: 1,
                  child: TextField(
                    controller: _dayController,
                    decoration: const InputDecoration(labelText: 'Day', border: OutlineInputBorder()),
                    keyboardType: const TextInputType.numberWithOptions(decimal: true),
                  ),
                ),
              ],
            ),
            const SizedBox(height: 16),
            Text(
              'Julian Day: $_resultJd',
              style: const TextStyle(fontSize: 20, color: Colors.lightGreenAccent),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildFooter() {
    return const Column(
      children: [
        Text('ALL RIGHTS RESERVED BY'),
        Text('S&T SOFTWARE SERVICE'),
        SizedBox(height: 16),
        Text(
          'Note: Handles both Julian and Gregorian calendars.\nTransition date: Oct 15, 1582.',
          textAlign: TextAlign.center,
          style: TextStyle(fontSize: 12, color: Colors.grey),
        ),
      ],
    );
  }
}
