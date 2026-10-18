import 'dart:math' as math;
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:google_sign_in/google_sign_in.dart';
import '../../../core/providers/firebase_providers.dart';
import '../../../core/providers/google_auth_providers.dart';

class LoginPage extends ConsumerStatefulWidget {
  const LoginPage({super.key});

  @override
  ConsumerState<LoginPage> createState() => _LoginPageState();
}

class _LoginPageState extends ConsumerState<LoginPage> {
  final _formKey = GlobalKey<FormState>();
  final _emailController = TextEditingController();
  final _passwordController = TextEditingController();
  bool _isLoading = false;
  bool _isGoogleLoading = false;
  String? _errorMessage;

  @override
  void dispose() {
    _emailController.dispose();
    _passwordController.dispose();
    super.dispose();
  }

  Future<void> _signInWithEmail() async {
    if (!_formKey.currentState!.validate()) return;

    setState(() {
      _isLoading = true;
      _errorMessage = null;
    });

    try {
      final auth = ref.read(firebaseAuthProvider);
      await auth.signInWithEmailAndPassword(
        email: _emailController.text.trim(),
        password: _passwordController.text.trim(),
      );
      if (mounted) context.go('/calendar');
    } catch (e) {
      try {
        final auth = ref.read(firebaseAuthProvider);
        await auth.createUserWithEmailAndPassword(
          email: _emailController.text.trim(),
          password: _passwordController.text.trim(),
        );
        if (mounted) context.go('/calendar');
      } catch (createErr) {
        if (mounted) {
          setState(() {
            _errorMessage = createErr.toString();
          });
        }
      }
    } finally {
      if (mounted) setState(() => _isLoading = false);
    }
  }

  Future<void> _signInAnonymously() async {
    setState(() {
      _isLoading = true;
      _errorMessage = null;
    });

    try {
      final auth = ref.read(firebaseAuthProvider);
      await auth.signInAnonymously();
      if (mounted) context.go('/calendar');
    } catch (e) {
      if (mounted) {
        setState(() {
          _errorMessage = e.toString();
        });
      }
    } finally {
      if (mounted) setState(() => _isLoading = false);
    }
  }

  Future<void> _signInWithGoogle() async {
    setState(() {
      _isGoogleLoading = true;
      _errorMessage = null;
    });

    try {
      await ref.read(googleAuthServiceProvider).signIn();
      if (mounted) context.go('/calendar');
    } on GoogleSignInException catch (e) {
      if (!mounted) return;
      setState(() => _errorMessage = _describeGoogleError(e));
    } on UnsupportedError catch (e) {
      if (!mounted) return;
      setState(() => _errorMessage = '${e.message}');
    } catch (e) {
      if (!mounted) return;
      setState(() => _errorMessage = 'Google sign-in failed: $e');
    } finally {
      if (mounted) setState(() => _isGoogleLoading = false);
    }
  }

  /// Turns the plugin's exception codes into something an observer can act on.
  String _describeGoogleError(GoogleSignInException e) {
    switch (e.code) {
      case GoogleSignInExceptionCode.canceled:
        return 'Google sign-in was canceled.';
      case GoogleSignInExceptionCode.clientConfigurationError:
        return 'Google sign-in is not configured for this build. See '
            'docs/GOOGLE_SIGN_IN.md for the Firebase console setup.';
      case GoogleSignInExceptionCode.uiUnavailable:
        return 'The Google sign-in window could not be shown. Check that '
            'popups are allowed for this site.';
      default:
        return e.description ?? 'Google sign-in failed (${e.code.name}).';
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Account Sign In'),
      ),
      body: Center(
        child: SingleChildScrollView(
          padding: const EdgeInsets.all(24.0),
          child: Container(
            constraints: const BoxConstraints(maxWidth: 400),
            child: Form(
              key: _formKey,
              child: Column(
                mainAxisAlignment: MainAxisAlignment.center,
                crossAxisAlignment: CrossAxisAlignment.stretch,
                children: [
                  Icon(
                    Icons.nights_stay,
                    size: 72,
                    color: Theme.of(context).colorScheme.primary,
                  ),
                  const SizedBox(height: 16),
                  Text(
                    'AstroDiary Sync',
                    textAlign: TextAlign.center,
                    style: Theme.of(context).textTheme.headlineMedium?.copyWith(
                          fontWeight: FontWeight.bold,
                        ),
                  ),
                  const SizedBox(height: 8),
                  Text(
                    'Sign in to synchronize your observation logs across devices.',
                    textAlign: TextAlign.center,
                    style: Theme.of(context).textTheme.bodyMedium,
                  ),
                  const SizedBox(height: 32),
                  if (_errorMessage != null) ...[
                    Container(
                      padding: const EdgeInsets.all(12),
                      decoration: BoxDecoration(
                        color: Colors.red.withValues(alpha: 0.15),
                        borderRadius: BorderRadius.circular(8),
                        border: Border.all(color: Colors.red),
                      ),
                      child: Text(
                        _errorMessage!,
                        style: const TextStyle(color: Colors.red),
                      ),
                    ),
                    const SizedBox(height: 16),
                  ],
                  TextFormField(
                    controller: _emailController,
                    keyboardType: TextInputType.emailAddress,
                    decoration: const InputDecoration(
                      labelText: 'Email Address',
                      prefixIcon: Icon(Icons.email),
                      border: OutlineInputBorder(),
                    ),
                    validator: (value) =>
                        value == null || !value.contains('@')
                            ? 'Enter a valid email address'
                            : null,
                  ),
                  const SizedBox(height: 16),
                  TextFormField(
                    controller: _passwordController,
                    obscureText: true,
                    decoration: const InputDecoration(
                      labelText: 'Password',
                      prefixIcon: Icon(Icons.lock),
                      border: OutlineInputBorder(),
                    ),
                    validator: (value) => value == null || value.length < 6
                        ? 'Password must be at least 6 characters'
                        : null,
                  ),
                  const SizedBox(height: 24),
                  if (isGoogleSignInSupported) ...[
                    _GoogleSignInButton(
                      isLoading: _isGoogleLoading,
                      enabled: !_isLoading && !_isGoogleLoading,
                      onPressed: _signInWithGoogle,
                    ),
                    const SizedBox(height: 16),
                    Row(
                      children: [
                        const Expanded(child: Divider()),
                        Padding(
                          padding: const EdgeInsets.symmetric(horizontal: 12),
                          child: Text(
                            'or',
                            style: Theme.of(context).textTheme.bodySmall,
                          ),
                        ),
                        const Expanded(child: Divider()),
                      ],
                    ),
                    const SizedBox(height: 16),
                  ],
                  if (_isLoading || _isGoogleLoading)
                    const Center(child: CircularProgressIndicator())
                  else ...[
                    ElevatedButton.icon(
                      style: ElevatedButton.styleFrom(
                        minimumSize: const Size(double.infinity, 48),
                      ),
                      onPressed: _signInWithEmail,
                      icon: const Icon(Icons.login),
                      label: const Text('Sign In / Register'),
                    ),
                    const SizedBox(height: 12),
                    OutlinedButton.icon(
                      style: OutlinedButton.styleFrom(
                        minimumSize: const Size(double.infinity, 48),
                      ),
                      onPressed: _signInAnonymously,
                      icon: const Icon(Icons.person_outline),
                      label: const Text('Continue as Guest'),
                    ),
                  ],
                ],
              ),
            ),
          ),
        ),
      ),
    );
  }
}

/// "Continue with Google" button.
///
/// `google_sign_in` v7 no longer ships a button widget, so this draws one. The
/// Google "G" is rendered with the official brand colours.
class _GoogleSignInButton extends StatelessWidget {
  const _GoogleSignInButton({
    required this.isLoading,
    required this.enabled,
    required this.onPressed,
  });

  final bool isLoading;
  final bool enabled;
  final VoidCallback onPressed;

  @override
  Widget build(BuildContext context) {
    return OutlinedButton(
      style: OutlinedButton.styleFrom(
        minimumSize: const Size(double.infinity, 48),
        side: const BorderSide(color: Color(0xFFDADCE0)),
        foregroundColor: const Color(0xFF1F1F1F),
      ),
      onPressed: enabled ? onPressed : null,
      child: isLoading
          ? const SizedBox(
              width: 20,
              height: 20,
              child: CircularProgressIndicator(strokeWidth: 2),
            )
          : const Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                _GoogleLogo(),
                SizedBox(width: 12),
                Text('Continue with Google'),
              ],
            ),
    );
  }
}

/// The four-colour Google "G".
class _GoogleLogo extends StatelessWidget {
  const _GoogleLogo();

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      width: 20,
      height: 20,
      child: CustomPaint(painter: _GoogleLogoPainter()),
    );
  }
}

class _GoogleLogoPainter extends CustomPainter {
  @override
  void paint(ui.Canvas canvas, Size size) {
    final w = size.width;
    final stroke = w * 0.18;
    final radius = (w - stroke) / 2;
    final paint = ui.Paint()
      ..style = ui.PaintingStyle.stroke
      ..strokeWidth = stroke;

    canvas.save();
    canvas.translate(w / 2, w / 2);
    canvas.rotate(-math.pi / 2);

    // Four arcs, one per brand colour, each covering a quarter turn plus an
    // overlap so the ring has no visible seams.
    void arc(double startDeg, double sweepDeg, Color color) {
      paint.color = color;
      canvas.drawArc(
        ui.Rect.fromCircle(center: Offset.zero, radius: radius),
        startDeg * math.pi / 180,
        sweepDeg * math.pi / 180,
        false,
        paint,
      );
    }

    arc(-42, 96, const Color(0xFF4285F4)); // blue
    arc(54, 96, const Color(0xFF34A853)); // green
    arc(150, 96, const Color(0xFFFBBC05)); // yellow
    arc(246, 96, const Color(0xFFEA4335)); // red

    // Blue horizontal bar across the middle.
    paint.color = const Color(0xFF4285F4);
    paint.strokeCap = ui.StrokeCap.butt;
    canvas.drawLine(Offset.zero, Offset(radius, 0), paint);

    canvas.restore();
  }

  @override
  bool shouldRepaint(covariant CustomPainter oldDelegate) => false;
}
