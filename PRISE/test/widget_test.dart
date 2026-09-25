import 'package:flutter_test/flutter_test.dart';

import 'package:prise/main.dart';

void main() {
  testWidgets('shows the calculator and accessible navigation', (tester) async {
    await tester.pumpWidget(const PriseApp());
    await tester.pumpAndSettle();

    expect(find.text('Find a planet in the sky'), findsOneWidget);
    expect(find.text('Calculate rise and set'), findsOneWidget);
    expect(find.text('Instructions'), findsOneWidget);
    expect(find.text('About'), findsOneWidget);
  });
}
