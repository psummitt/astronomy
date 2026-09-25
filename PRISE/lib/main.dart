import 'dart:math' as math;

import 'package:flutter/material.dart';

void main() => runApp(const PriseApp());

class PriseApp extends StatelessWidget {
  const PriseApp({super.key});

  @override
  Widget build(BuildContext context) {
    const ink = Color(0xff102a43);
    const sea = Color(0xff0b7285);
    return MaterialApp(
      title: 'PRISE planetary rise and set',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        useMaterial3: true,
        colorScheme: ColorScheme.fromSeed(
          seedColor: sea,
          brightness: Brightness.light,
          primary: sea,
          onPrimary: Colors.white,
          surface: const Color(0xfff7fafc),
          onSurface: ink,
        ),
        scaffoldBackgroundColor: const Color(0xfff7fafc),
        textTheme: const TextTheme(
          bodyLarge: TextStyle(fontSize: 17, height: 1.45, color: ink),
          bodyMedium: TextStyle(fontSize: 16, height: 1.4, color: ink),
          titleLarge: TextStyle(
            fontSize: 24,
            fontWeight: FontWeight.w700,
            color: ink,
          ),
          headlineMedium: TextStyle(
            fontSize: 30,
            fontWeight: FontWeight.w800,
            color: ink,
          ),
        ),
        inputDecorationTheme: InputDecorationTheme(
          filled: true,
          fillColor: Colors.white,
          border: OutlineInputBorder(borderRadius: BorderRadius.circular(10)),
          enabledBorder: const OutlineInputBorder(
            borderSide: BorderSide(color: Color(0xff829ab1), width: 1.2),
            borderRadius: BorderRadius.all(Radius.circular(10)),
          ),
          focusedBorder: const OutlineInputBorder(
            borderSide: BorderSide(color: sea, width: 3),
            borderRadius: BorderRadius.all(Radius.circular(10)),
          ),
          labelStyle: const TextStyle(fontSize: 16, color: ink),
        ),
        navigationBarTheme: const NavigationBarThemeData(
          height: 76,
          labelTextStyle: WidgetStatePropertyAll(
            TextStyle(fontSize: 15, fontWeight: FontWeight.w600),
          ),
        ),
      ),
      home: const PriseShell(),
    );
  }
}

class PriseShell extends StatefulWidget {
  const PriseShell({super.key});
  @override
  State<PriseShell> createState() => _PriseShellState();
}

class _PriseShellState extends State<PriseShell> {
  int _page = 0;
  @override
  Widget build(BuildContext context) {
    const pages = [CalculatorPage(), InstructionsPage(), AboutPage()];
    return Scaffold(
      appBar: AppBar(
        title: const Text(
          'PRISE',
          style: TextStyle(fontWeight: FontWeight.w800, letterSpacing: 1.2),
        ),
        toolbarHeight: 72,
        backgroundColor: const Color(0xffd9f0f2),
        foregroundColor: const Color(0xff102a43),
      ),
      body: SafeArea(child: pages[_page]),
      bottomNavigationBar: NavigationBar(
        selectedIndex: _page,
        onDestinationSelected: (index) => setState(() => _page = index),
        destinations: const [
          NavigationDestination(
            icon: Icon(Icons.explore_outlined),
            selectedIcon: Icon(Icons.explore),
            label: 'Calculator',
          ),
          NavigationDestination(
            icon: Icon(Icons.menu_book_outlined),
            selectedIcon: Icon(Icons.menu_book),
            label: 'Instructions',
          ),
          NavigationDestination(
            icon: Icon(Icons.info_outline),
            selectedIcon: Icon(Icons.info),
            label: 'About',
          ),
        ],
      ),
    );
  }
}

class CalculatorPage extends StatefulWidget {
  const CalculatorPage({super.key});
  @override
  State<CalculatorPage> createState() => _CalculatorPageState();
}

class _CalculatorPageState extends State<CalculatorPage> {
  final _latitude = TextEditingController(text: '40.0');
  final _longitude = TextEditingController(text: '-75.0');
  DateTime _date = DateTime.now();
  String _body = 'Mercury';
  RiseSetResult? _result;
  String? _error;

  @override
  void initState() {
    super.initState();
    WidgetsBinding.instance.addPostFrameCallback((_) => _calculate());
  }

  @override
  void dispose() {
    _latitude.dispose();
    _longitude.dispose();
    super.dispose();
  }

  Future<void> _pickDate() async {
    final chosen = await showDatePicker(
      context: context,
      initialDate: _date,
      firstDate: DateTime(1900),
      lastDate: DateTime(2100),
      helpText: 'Choose the date to calculate',
    );
    if (chosen != null && chosen != _date) {
      setState(() => _date = chosen);
      _calculate();
    }
  }

  void _calculate() {
    final latitude = double.tryParse(_latitude.text.trim());
    final longitude = double.tryParse(_longitude.text.trim());
    if (latitude == null || latitude < -90 || latitude > 90) {
      setState(
        () => _error = 'Latitude must be a number from -90 to 90 degrees.',
      );
      return;
    }
    if (longitude == null || longitude < -180 || longitude > 180) {
      setState(
        () => _error = 'Longitude must be a number from -180 to 180 degrees.',
      );
      return;
    }
    setState(() {
      _error = null;
      _result = calculateSunRelativeRiseSet(
        _date,
        latitude,
        longitude,
        _body,
      );
    });
  }

  @override
  Widget build(BuildContext context) {
    return LayoutBuilder(
      builder: (context, constraints) {
        final wide = constraints.maxWidth >= 860;
        final form = _buildForm(context);
        final result = _buildResult(context);
        return SingleChildScrollView(
          padding: const EdgeInsets.fromLTRB(20, 24, 20, 36),
          child: Center(
            child: ConstrainedBox(
              constraints: const BoxConstraints(maxWidth: 1180),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    'Find Mercury or Venus near the Sun',
                    style: Theme.of(context).textTheme.headlineMedium,
                  ),
                  const SizedBox(height: 8),
                  const Text(
                    'Estimate when either inner planet rises or sets relative to the Sun.',
                    style: TextStyle(fontSize: 18),
                  ),
                  const SizedBox(height: 22),
                  if (wide)
                    Row(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Expanded(child: form),
                        const SizedBox(width: 24),
                        Expanded(child: result),
                      ],
                    )
                  else ...[
                    form,
                    const SizedBox(height: 24),
                    result,
                  ],
                ],
              ),
            ),
          ),
        );
      },
    );
  }

  Widget _buildForm(BuildContext context) {
    return Card(
      elevation: 0,
      color: const Color(0xffe8f4f4),
      shape: RoundedRectangleBorder(
        borderRadius: BorderRadius.circular(14),
        side: const BorderSide(color: Color(0xffb7d9dc)),
      ),
      child: Padding(
        padding: const EdgeInsets.all(22),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'Observation details',
              style: Theme.of(context).textTheme.titleLarge,
            ),
            const SizedBox(height: 18),
            DropdownButtonFormField<String>(
              initialValue: _body,
              decoration: const InputDecoration(
                labelText: 'Planet',
                helperText: 'Select Mercury or Venus.',
              ),
              items:
                  const [
                        'Mercury',
                        'Venus',
                      ]
                      .map(
                        (body) =>
                            DropdownMenuItem(value: body, child: Text(body)),
                      )
                      .toList(),
              onChanged: (value) {
                if (value == null || value == _body) return;
                setState(() => _body = value);
                _calculate();
              },
            ),
            const SizedBox(height: 16),
            Semantics(
              button: true,
              label: 'Date, ${_date.year}-${_date.month}-${_date.day}',
              child: InkWell(
                onTap: _pickDate,
                borderRadius: BorderRadius.circular(10),
                child: InputDecorator(
                  decoration: const InputDecoration(
                    labelText: 'Date',
                    suffixIcon: Icon(Icons.calendar_today),
                    helperText: 'Use the calendar to choose a date.',
                  ),
                  child: Text(
                    '${_date.year}-${_date.month.toString().padLeft(2, '0')}-${_date.day.toString().padLeft(2, '0')}',
                  ),
                ),
              ),
            ),
            const SizedBox(height: 16),
            Row(
              children: [
                Expanded(
                  child: TextField(
                    controller: _latitude,
                    keyboardType: const TextInputType.numberWithOptions(
                      decimal: true,
                      signed: true,
                    ),
                    decoration: const InputDecoration(
                      labelText: 'Latitude (degrees)',
                      helperText: 'North +, south -',
                    ),
                  ),
                ),
                const SizedBox(width: 14),
                Expanded(
                  child: TextField(
                    controller: _longitude,
                    keyboardType: const TextInputType.numberWithOptions(
                      decimal: true,
                      signed: true,
                    ),
                    decoration: const InputDecoration(
                      labelText: 'Longitude (degrees)',
                      helperText: 'East +, west -',
                    ),
                  ),
                ),
              ],
            ),
            const SizedBox(height: 16),
            const SizedBox(height: 22),
            SizedBox(
              width: double.infinity,
              child: FilledButton.icon(
                onPressed: _calculate,
                icon: const Icon(Icons.calculate),
                label: const Padding(
                  padding: EdgeInsets.symmetric(vertical: 12),
                  child: Text(
                    'Update estimate',
                    style: TextStyle(fontSize: 17),
                  ),
                ),
              ),
            ),
            if (_error != null) ...[
              const SizedBox(height: 14),
              Text(
                _error!,
                style: const TextStyle(
                  color: Color(0xffa61b1b),
                  fontWeight: FontWeight.w700,
                ),
              ),
            ],
          ],
        ),
      ),
    );
  }

  Widget _buildResult(BuildContext context) {
    final result = _result;
    return Card(
      elevation: 0,
      color: Colors.white,
      shape: RoundedRectangleBorder(
        borderRadius: BorderRadius.circular(14),
        side: const BorderSide(color: Color(0xffbcccdc)),
      ),
      child: Padding(
        padding: const EdgeInsets.all(22),
        child: result == null
            ? const SizedBox(
                height: 220,
                child: Center(
                  child: Text('Enter your details and calculate a result.'),
                ),
              )
            : Semantics(
                liveRegion: true,
                label: result.accessibleSummary,
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text(
                      '${result.body} on ${result.dateLabel}',
                      key: const ValueKey('result-heading'),
                      style: Theme.of(context).textTheme.titleLarge,
                    ),
                    const SizedBox(height: 6),
                    Text(result.locationLabel),
                    const SizedBox(height: 22),
                    Text(
                      result.eventTitle,
                      style: Theme.of(context).textTheme.titleLarge,
                    ),
                    const SizedBox(height: 8),
                    Text(
                      result.eventDetail,
                      key: const ValueKey('result-event-detail'),
                      style: Theme.of(context).textTheme.headlineMedium,
                    ),
                    const Divider(height: 28),
                    Container(
                      width: double.infinity,
                      padding: const EdgeInsets.all(14),
                      decoration: BoxDecoration(
                        color: const Color(0xfffff4d6),
                        borderRadius: BorderRadius.circular(10),
                      ),
                      child: Text(
                        result.note,
                        style: const TextStyle(fontWeight: FontWeight.w600),
                      ),
                    ),
                    const SizedBox(height: 18),
                    const Text(
                      'Times are approximate and intended for planning observations.',
                      style: TextStyle(fontSize: 14),
                    ),
                  ],
                ),
              ),
      ),
    );
  }
}

class InstructionsPage extends StatelessWidget {
  const InstructionsPage({super.key});
  @override
  Widget build(BuildContext context) => const _InfoPage(
    title: 'Instructions',
    icon: Icons.menu_book,
    children: [
      _InfoSection(
        title: '1. Choose a planet',
        body: 'Select Mercury or Venus to estimate its rise or set relative to the Sun.',
      ),
      _InfoSection(
        title: '2. Choose a date',
        body: 'Use the Date field to select the calendar date for the estimate.',
      ),
      _InfoSection(
        title: '3. Enter your location',
        body: 'Latitude is positive north of the equator and negative south. Longitude is positive east of Greenwich and negative west.',
      ),
      _InfoSection(
        title: '4. Read the result',
        body: 'The estimate reports how many minutes the planet rises before sunrise or sets after sunset, based on whether it is a morning or evening star.',
      ),
      _InfoSection(
        title: 'Accuracy',
        body: 'PRISE uses compact orbital elements and a standard horizon model. Refraction, terrain, and atmospheric conditions are not modeled. Use a professional ephemeris for navigation or scientific work.',
      ),
    ],
  );
}

class AboutPage extends StatelessWidget {
  const AboutPage({super.key});
  @override
  Widget build(BuildContext context) => const _InfoPage(
    title: 'About PRISE',
    icon: Icons.info,
    children: [
      _InfoSection(
        title: 'Planetary rise and set',
        body: 'PRISE is a modern Flutter adaptation of the PRISE.BAS astronomy program by Eric Burgess.',
      ),
      _InfoSection(
        title: 'Source note',
        body: 'PRISE.BAS estimates when Mercury and Venus rise before the Sun as morning stars or set after it as evening stars. This app follows that purpose with a modern low-precision orbital model; its calculations are not a line-for-line port of the BASIC listing.',
      ),
      _InfoSection(
        title: 'Accessibility',
        body: 'The interface uses semantic labels, a logical focus order, keyboard-accessible controls, high-contrast colors, large touch targets, readable type, and live result announcements. It supports platform text scaling without relying on color alone.',
      ),
      _InfoSection(
        title: 'Platforms',
        body: 'Built with Flutter for Android, Linux, and the web. No network connection is required for calculations.',
      ),
    ],
  );
}

class _InfoPage extends StatelessWidget {
  const _InfoPage({
    required this.title,
    required this.icon,
    required this.children,
  });
  final String title;
  final IconData icon;
  final List<Widget> children;

  @override
  Widget build(BuildContext context) => SingleChildScrollView(
    padding: const EdgeInsets.fromLTRB(20, 26, 20, 38),
    child: Center(
      child: ConstrainedBox(
        constraints: const BoxConstraints(maxWidth: 820),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                Icon(icon, size: 34, color: const Color(0xff0b7285)),
                const SizedBox(width: 12),
                Text(title, style: Theme.of(context).textTheme.headlineMedium),
              ],
            ),
            const SizedBox(height: 24),
            ...children,
          ],
        ),
      ),
    ),
  );
}

class _InfoSection extends StatelessWidget {
  const _InfoSection({required this.title, required this.body});
  final String title;
  final String body;
  @override
  Widget build(BuildContext context) => Padding(
    padding: const EdgeInsets.only(bottom: 24),
    child: Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(title, style: Theme.of(context).textTheme.titleLarge),
        const SizedBox(height: 6),
        Text(body),
      ],
    ),
  );
}

class RiseSetResult {
  const RiseSetResult({
    required this.body,
    required this.dateLabel,
    required this.locationLabel,
    required this.eventTitle,
    required this.eventDetail,
    required this.note,
    required this.accessibleSummary,
  });
  final String body;
  final String dateLabel;
  final String locationLabel;
  final String eventTitle;
  final String eventDetail;
  final String note;
  final String accessibleSummary;
}

RiseSetResult calculateSunRelativeRiseSet(
  DateTime date,
  double latitude,
  double longitude,
  String body,
) {
  final jd = _julianDay(DateTime.utc(date.year, date.month, date.day));
  final planet = _bodyPosition(body, jd);
  final sun = _bodyPosition('Sun', jd);
  final planetEvents = _horizonEvents(body, planet, jd, latitude, longitude);
  final sunEvents = _horizonEvents('Sun', sun, jd, latitude, longitude);
  final label =
      '${date.year}-${date.month.toString().padLeft(2, '0')}-${date.day.toString().padLeft(2, '0')}';
  final location =
      '${latitude.toStringAsFixed(2)}°, ${longitude.toStringAsFixed(2)}°';
  if (planetEvents == null || sunEvents == null) {
    const note = 'A horizon crossing could not be estimated for this location and date.';
    return RiseSetResult(
      body: body,
      dateLabel: label,
      locationLabel: location,
      eventTitle: 'No horizon estimate',
      eventDetail: '',
      note: note,
      accessibleSummary: '$body on $label. $note',
    );
  }

  final isEveningStar = _signedHours(planet.ra - sun.ra) > 0;
  final offsetHours = isEveningStar
      ? _signedHours(planetEvents.set - sunEvents.set)
      : _signedHours(sunEvents.rise - planetEvents.rise);
  final offsetMinutes = offsetHours * 60;
    final relation = isEveningStar
      ? (offsetMinutes >= 0 ? 'after sunset' : 'before sunset')
      : (offsetMinutes >= 0 ? 'before sunrise' : 'after sunrise');
    final eventDetail =
      '${isEveningStar ? 'Sets' : 'Rises'} ${offsetMinutes.abs().round()} minutes $relation';
  final eventTitle = isEveningStar ? 'Evening star' : 'Morning star';
  const note = 'Approximate timing based on the selected date and observing location.';
  return RiseSetResult(
    body: body,
    dateLabel: label,
    locationLabel: location,
    eventTitle: eventTitle,
    eventDetail: eventDetail,
    note: note,
    accessibleSummary: '$body on $label. $eventTitle. $eventDetail.',
  );
}

class _HorizonEvents {
  const _HorizonEvents(this.rise, this.set);
  final double rise;
  final double set;
}

_HorizonEvents? _horizonEvents(
  String body,
  _Position position,
  double jd,
  double latitude,
  double longitude,
) {
  final lat = latitude * math.pi / 180;
  final dec = position.dec * math.pi / 180;
  final altitude = body == 'Sun' ? -0.833 : -0.566;
  final cosH =
      (math.sin(altitude * math.pi / 180) - math.sin(lat) * math.sin(dec)) /
      (math.cos(lat) * math.cos(dec));
  if (cosH > 1 || cosH < -1) return null;
  final hourAngle = math.acos(cosH) * 12 / math.pi;
  final gmst = (6.697374558 + 0.06570982441908 * (jd - 2451545.0)) % 24;
  final transit = ((position.ra - gmst - longitude / 15) / 1.00273790935) % 24;
  final rise = (transit - hourAngle / 1.00273790935) % 24;
  final set = (transit + hourAngle / 1.00273790935) % 24;
  return _HorizonEvents(rise, set);
}

double _signedHours(double value) {
  var hours = (value + 12) % 24;
  if (hours < 0) hours += 24;
  return hours - 12;
}

double _julianDay(DateTime date) =>
    date.millisecondsSinceEpoch / 86400000 + 2440587.5;

class _Position {
  const _Position(this.ra, this.dec);
  final double ra;
  final double dec;
}

_Position _bodyPosition(String body, double jd) {
  final d = jd - 2451543.5;
  if (body == 'Sun') {
    final w = 282.9404 + 4.70935e-5 * d;
    final e = 0.016709 - 1.151e-9 * d;
    final m = _normalize(356.0470 + 0.9856002585 * d) * math.pi / 180;
    final eccentric = m + e * math.sin(m) * (1 + e * math.cos(m));
    final xv = math.cos(eccentric) - e;
    final yv = math.sqrt(1 - e * e) * math.sin(eccentric);
    return _equatorial(math.atan2(yv, xv) * 180 / math.pi + w, 0, d);
  }
  final elements = <String, List<double>>{
    'Mercury': [
      48.3313,
      3.24587e-5,
      7.0047,
      5.00e-8,
      29.1241,
      1.01444e-5,
      0.387098,
      0,
      0.205635,
      5.59e-10,
      168.6562,
      4.0923344368,
    ],
    'Venus': [
      76.6799,
      2.46590e-5,
      3.3946,
      2.75e-8,
      54.8910,
      1.38374e-5,
      0.72333,
      0,
      0.006773,
      -1.302e-9,
      48.0052,
      1.6021302244,
    ],
    'Mars': [
      49.5574,
      2.11081e-5,
      1.8497,
      -1.78e-8,
      286.5016,
      2.92961e-5,
      1.523688,
      0,
      0.093405,
      2.516e-9,
      18.6021,
      0.5240207766,
    ],
    'Jupiter': [
      100.4542,
      2.76854e-5,
      1.3030,
      -1.557e-7,
      273.8777,
      1.64505e-5,
      5.20256,
      0,
      0.048498,
      4.469e-9,
      19.8950,
      0.0830853001,
    ],
    'Saturn': [
      113.6634,
      2.38980e-5,
      2.4886,
      -1.081e-7,
      339.3939,
      2.97661e-5,
      9.55475,
      0,
      0.055546,
      -9.499e-9,
      316.9670,
      0.0334442282,
    ],
    'Uranus': [
      74.0005,
      1.3978e-5,
      0.7733,
      1.9e-8,
      96.6612,
      3.0565e-5,
      19.18171,
      -1.55e-8,
      0.047318,
      7.45e-9,
      142.5905,
      0.011725806,
    ],
    'Neptune': [
      131.7806,
      3.0173e-5,
      1.7700,
      -2.55e-7,
      272.8461,
      -6.027e-6,
      30.05826,
      3.313e-8,
      0.008606,
      2.15e-9,
      260.2471,
      0.005995147,
    ],
  }[body]!;
  final n = _normalize(elements[0] + elements[1] * d) * math.pi / 180;
  final i = (elements[2] + elements[3] * d) * math.pi / 180;
  final w = _normalize(elements[4] + elements[5] * d) * math.pi / 180;
  final a = elements[6];
  final e = elements[8] + elements[9] * d;
  final m = _normalize(elements[10] + elements[11] * d) * math.pi / 180;
  var eccentric = m;
  for (var step = 0; step < 5; step++) {
    eccentric = m + e * math.sin(eccentric);
  }
  final xv = a * (math.cos(eccentric) - e);
  final yv = a * (math.sqrt(1 - e * e) * math.sin(eccentric));
  final v = math.atan2(yv, xv);
  final r = math.sqrt(xv * xv + yv * yv);
  final xh =
      r *
      (math.cos(n) * math.cos(v + w) -
          math.sin(n) * math.sin(v + w) * math.cos(i));
  final yh =
      r *
      (math.sin(n) * math.cos(v + w) +
          math.cos(n) * math.sin(v + w) * math.cos(i));
  final zh = r * math.sin(v + w) * math.sin(i);
  final earth = _heliocentricEarth(d);
  final xg = xh - earth.x;
  final yg = yh - earth.y;
  final zg = zh;
  return _equatorial(
    math.atan2(yg, xg) * 180 / math.pi,
    math.atan2(zg, math.sqrt(xg * xg + yg * yg)) * 180 / math.pi,
    d,
  );
}

_Position _equatorial(double lon, double lat, double d) {
  final ecl = (23.4393 - 3.563e-7 * d) * math.pi / 180;
  final lambda = lon * math.pi / 180;
  final beta = lat * math.pi / 180;
  final ra =
      math.atan2(
        math.sin(lambda) * math.cos(ecl) - math.tan(beta) * math.sin(ecl),
        math.cos(lambda),
      ) *
      12 /
      math.pi;
  final dec =
      math.asin(
        math.sin(beta) * math.cos(ecl) +
            math.cos(beta) * math.sin(ecl) * math.sin(lambda),
      ) *
      180 /
      math.pi;
  return _Position((_normalize(ra * 15) / 15 + 24) % 24, dec);
}

double _normalize(double value) =>
    value % 360 < 0 ? value % 360 + 360 : value % 360;

class _Earth {
  const _Earth(this.x, this.y);
  final double x;
  final double y;
}

_Earth _heliocentricEarth(double d) {
  final m = _normalize(356.0470 + 0.9856002585 * d) * math.pi / 180;
  final e = 0.016709 - 1.151e-9 * d;
  final eccentric = m + e * math.sin(m) * (1 + e * math.cos(m));
  final w = (282.9404 + 4.70935e-5 * d) * math.pi / 180;
  final xv = math.cos(eccentric) - e;
  final yv = math.sqrt(1 - e * e) * math.sin(eccentric);
  final lon = math.atan2(yv, xv) + w;
  final r = math.sqrt(xv * xv + yv * yv);
  return _Earth(-r * math.cos(lon), -r * math.sin(lon));
}
