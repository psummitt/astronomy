import 'package:flutter_test/flutter_test.dart';
import 'package:provider/provider.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:calendar_app/main.dart';
import 'package:calendar_app/providers/calendar_provider.dart';

void main() {
  testWidgets('App smoke test', (WidgetTester tester) async {
    // Provide mock initial values for SharedPreferences
    SharedPreferences.setMockInitialValues({});
    
    // Build our app and trigger a frame.
    await tester.pumpWidget(
      ChangeNotifierProvider(
        create: (_) => CalendarProvider(),
        child: const PerpetualCalendarApp(),
      ),
    );

    // Let the async state loading finish and UI settle
    await tester.pump();
    await tester.pump(const Duration(milliseconds: 500));

    // Verify that the title is rendered.
    expect(find.text('Perpetual Calendar'), findsOneWidget);
  });
}
