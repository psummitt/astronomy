import 'package:flutter_test/flutter_test.dart';
import 'package:epoch/main.dart';

void main() {
  testWidgets('Epoch app loads', (WidgetTester tester) async {
    await tester.pumpWidget(const EpochApp());

    expect(find.text('EPOCH I'), findsOneWidget);
    expect(find.text('First Epoch (Year)'), findsOneWidget);
  });
}
