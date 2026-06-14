import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'providers/calendar_provider.dart';
import 'views/calendar_view.dart';
import 'views/day_of_week_view.dart';
import 'views/holiday_view.dart';

void main() {
  WidgetsFlutterBinding.ensureInitialized();
  runApp(
    ChangeNotifierProvider(
      create: (_) => CalendarProvider(),
      child: const PerpetualCalendarApp(),
    ),
  );
}

class PerpetualCalendarApp extends StatelessWidget {
  const PerpetualCalendarApp({super.key});

  @override
  Widget build(BuildContext context) {
    // Modern Premium Theme
    final theme = ThemeData(
      brightness: Brightness.dark,
      scaffoldBackgroundColor: const Color(0xFF0B0F19),
      primaryColor: const Color(0xFF00F2FE),
      cardColor: const Color(0xFF161E31),
      dividerColor: Colors.white12,
      colorScheme: const ColorScheme.dark(
        primary: Color(0xFF00F2FE),
        onPrimary: Color(0xFF0B0F19),
        secondary: Color(0xFF4FACFE),
        onSecondary: Colors.white,
        surface: Color(0xFF161E31),
        onSurface: Color(0xFFE2E8F0),
      ),
      textTheme: const TextTheme(
        bodyLarge: TextStyle(fontFamily: 'Inter', color: Color(0xFFE2E8F0)),
        bodyMedium: TextStyle(fontFamily: 'Inter', color: Color(0xFF94A3B8)),
        titleLarge: TextStyle(fontFamily: 'Outfit', color: Colors.white, fontWeight: FontWeight.bold),
        titleMedium: TextStyle(fontFamily: 'Outfit', color: Color(0xFFE2E8F0)),
      ),
      elevatedButtonTheme: ElevatedButtonThemeData(
        style: ElevatedButton.styleFrom(
          foregroundColor: const Color(0xFF0B0F19),
          backgroundColor: const Color(0xFF00F2FE),
          textStyle: const TextStyle(fontFamily: 'Outfit', fontWeight: FontWeight.bold),
          shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(12)),
          padding: const EdgeInsets.symmetric(horizontal: 24, vertical: 14),
        ),
      ),
    );

    return MaterialApp(
      title: 'Perpetual Calendar',
      debugShowCheckedModeBanner: false,
      theme: theme,
      home: const CalendarAppHome(),
    );
  }
}

class CalendarAppHome extends StatefulWidget {
  const CalendarAppHome({super.key});

  @override
  State<CalendarAppHome> createState() => _CalendarAppHomeState();
}

class _CalendarAppHomeState extends State<CalendarAppHome> {
  int _currentIndex = 0;

  final List<Widget> _views = const [
    CalendarView(),
    DayOfWeekView(),
    HolidayView(),
    AboutView(),
  ];

  @override
  Widget build(BuildContext context) {
    final provider = Provider.of<CalendarProvider>(context);

    if (!provider.isLoaded) {
      return const Scaffold(
        body: Center(
          child: CircularProgressIndicator(),
        ),
      );
    }

    return LayoutBuilder(
      builder: (context, constraints) {
        final isWide = constraints.maxWidth >= 760;

        return Scaffold(
          appBar: AppBar(
            backgroundColor: const Color(0xFF0B0F19),
            elevation: 0,
            title: const Text(
              'Perpetual Calendar',
              style: TextStyle(
                fontSize: 24,
                fontWeight: FontWeight.w800,
                letterSpacing: 0.5,
                color: Colors.white,
              ),
            ),
          ),
          body: Row(
            children: [
              if (isWide) ...[
                // Sidebar Navigation for Desktop/Web
                NavigationRail(
                  selectedIndex: _currentIndex,
                  onDestinationSelected: (index) {
                    setState(() {
                      _currentIndex = index;
                    });
                  },
                  backgroundColor: const Color(0xFF0D1424),
                  indicatorColor: const Color(0xFF00F2FE).withValues(alpha: 0.2),
                  labelType: NavigationRailLabelType.all,
                  minWidth: 80,
                  destinations: const [
                    NavigationRailDestination(
                      icon: Icon(Icons.calendar_month),
                      selectedIcon: Icon(Icons.calendar_month, color: Color(0xFF00F2FE)),
                      label: Text('Calendar', style: TextStyle(fontFamily: 'Outfit')),
                    ),
                    NavigationRailDestination(
                      icon: Icon(Icons.question_mark),
                      selectedIcon: Icon(Icons.question_mark, color: Color(0xFF00F2FE)),
                      label: Text('Day Finder', style: TextStyle(fontFamily: 'Outfit')),
                    ),
                    NavigationRailDestination(
                      icon: Icon(Icons.celebration),
                      selectedIcon: Icon(Icons.celebration, color: Color(0xFF00F2FE)),
                      label: Text('Holidays', style: TextStyle(fontFamily: 'Outfit')),
                    ),
                    NavigationRailDestination(
                      icon: Icon(Icons.info_outline),
                      selectedIcon: Icon(Icons.info_outline, color: Color(0xFF00F2FE)),
                      label: Text('History', style: TextStyle(fontFamily: 'Outfit')),
                    ),
                  ],
                ),
                const VerticalDivider(width: 1, thickness: 1),
              ],
              Expanded(
                child: Center(
                  child: ConstrainedBox(
                    constraints: const BoxConstraints(maxWidth: 900),
                    child: Padding(
                      padding: const EdgeInsets.symmetric(horizontal: 16.0, vertical: 8.0),
                      child: IndexedStack(
                        index: _currentIndex,
                        children: _views,
                      ),
                    ),
                  ),
                ),
              ),
            ],
          ),
          bottomNavigationBar: !isWide
              ? BottomNavigationBar(
                  currentIndex: _currentIndex,
                  onTap: (index) {
                    setState(() {
                      _currentIndex = index;
                    });
                  },
                  backgroundColor: const Color(0xFF0D1424),
                  selectedItemColor: const Color(0xFF00F2FE),
                  unselectedItemColor: const Color(0xFF64748B),
                  type: BottomNavigationBarType.fixed,
                  items: const [
                    BottomNavigationBarItem(
                      icon: Icon(Icons.calendar_month),
                      label: 'Calendar',
                    ),
                    BottomNavigationBarItem(
                      icon: Icon(Icons.question_mark),
                      label: 'Day Finder',
                    ),
                    BottomNavigationBarItem(
                      icon: Icon(Icons.celebration),
                      label: 'Holidays',
                    ),
                    BottomNavigationBarItem(
                      icon: Icon(Icons.info_outline),
                      label: 'History',
                    ),
                  ],
                )
              : null,
        );
      },
    );
  }
}

class AboutView extends StatelessWidget {
  const AboutView({super.key});

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          const SizedBox(height: 16),
          // Tribute Header Card
          Container(
            width: double.infinity,
            padding: const EdgeInsets.all(24),
            decoration: BoxDecoration(
              color: const Color(0xFF1E293B),
              border: Border.all(
                color: const Color(0xFF38BDF8).withValues(alpha: 0.2),
                width: 1,
              ),
              borderRadius: BorderRadius.circular(16),
              gradient: const LinearGradient(
                colors: [Color(0xFF1E293B), Color(0xFF0F172A)],
                begin: Alignment.topLeft,
                end: Alignment.bottomRight,
              ),
            ),
            child: const Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  'HISTORICAL TRIBUTE',
                  style: TextStyle(
                    fontSize: 14,
                    color: Color(0xFF38BDF8),
                    fontWeight: FontWeight.bold,
                    letterSpacing: 1.5,
                  ),
                ),
                SizedBox(height: 12),
                Text(
                  'A Perpetual Calendar, Day-of-Week, and Holiday Program',
                  style: TextStyle(
                    fontSize: 22,
                    fontWeight: FontWeight.bold,
                    color: Colors.white,
                    fontFamily: 'Outfit',
                  ),
                ),
                SizedBox(height: 8),
                Text(
                  'Originally compiled by Eric Burgess F.R.A.S. (Fellow of the Royal Astronomical Society) circa 1980 for early microcomputer BASIC environments.',
                  style: TextStyle(
                    fontSize: 16,
                    color: Color(0xFF94A3B8),
                    height: 1.5,
                  ),
                ),
              ],
            ),
          ),
          const SizedBox(height: 24),
          // History Section
          const Text(
            'Program Background',
            style: TextStyle(
              fontSize: 18,
              fontWeight: FontWeight.bold,
              color: Colors.white,
              fontFamily: 'Outfit',
            ),
          ),
          const Divider(),
          const SizedBox(height: 8),
          const Text(
            'The original `Calendar.bas` script was written to run on early personal computers like the Apple II. It provided three main functions: generating visual calendar grids for a selected month, checking the day of the week for historical dates, and listing annual holidays.\n\n'
            'It used modular GOSUB subroutines and standard BASIC memory arrays (`DIM M(12)`) alongside raw screen plotting tricks (`VTAB`, `TAB`) to design interactive CLI pages on a standard 40-column CRT screen.\n\n'
            'This Flutter implementation pays homage to those pioneering days of personal computing, providing the exact historical routines wrapped in a clean responsive experience for web, desktop, and mobile.',
            style: TextStyle(
              fontSize: 15,
              height: 1.6,
              color: Color(0xFF94A3B8),
            ),
          ),
          const SizedBox(height: 24),
          const Text(
            'Technical Specifications',
            style: TextStyle(
              fontSize: 18,
              fontWeight: FontWeight.bold,
              color: Colors.white,
              fontFamily: 'Outfit',
            ),
          ),
          const Divider(),
          const SizedBox(height: 8),
          _buildBullet(context, 'Date limits: Validated for years after 1752, representing the calendar reform of Great Britain and its colonies.'),
          _buildBullet(context, 'Gregorian algorithms: Incorporates the Meeus/Jones/Butcher method for calculating Easter and related liturgical feasts (Ash Wednesday and Good Friday).'),
          _buildBullet(context, 'Adaptive UI: Automatically resizes from mobile aspect ratios up to horizontal widescreen desktop monitors.'),
          const SizedBox(height: 40),
        ],
      ),
    );
  }

  static Widget _buildBullet(BuildContext context, String text) {
    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 6.0),
      child: Row(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          const Text(
            '• ',
            style: TextStyle(
              color: Color(0xFF00F2FE),
              fontWeight: FontWeight.bold,
            ),
          ),
          Expanded(
            child: Text(
              text,
              style: const TextStyle(
                fontSize: 14,
                color: Color(0xFFE2E8F0),
                height: 1.4,
              ),
            ),
          ),
        ],
      ),
    );
  }
}
