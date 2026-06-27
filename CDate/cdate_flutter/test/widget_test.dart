import 'package:flutter_test/flutter_test.dart';
import 'package:cdate_flutter/main.dart';

void main() {
  testWidgets('App smoke test', (WidgetTester tester) async {
    // Build our app and trigger a frame.
    await tester.pumpWidget(const CDateApp());

    // Verify that the title is present.
    expect(find.text('CDate: Julian Day Converter'), findsOneWidget);
    expect(find.text('AN ASTRONOMY PROGRAM'), findsOneWidget);
  });
}
