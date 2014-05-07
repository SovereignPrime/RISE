# Maintainer:  T0ha aka Shvein Anton <t0hashvein@gmail.com>

pkgname=RISE
pkgver=0.0.29
pkgrel=1
pkgdesc="Secure sharing and collaboration system"
arch=('any')
url="http://sovereignprime.com"
license=('GPL2')
depends=('python2-gobject' 'webkitgtk' 'pywebkitgtk')
makedepends=('erlang>=17.0')
source=($pkgname-$pkgver.tgz::https://github.com/SovereignPrime/$pkgname/archive/v$pkgver.tar.gz)
sha256sums=('47b61664a47d29f3d5d4a3d6f01a7c5c13c1610767b6ed9e48c07439003533b8')

build() {
  cd "$srcdir/$pkgname-$pkgver"
  python2 setup.py build
}

package() {
  cd "$srcdir/$pkgname-$pkgver"
  python2 setup.py install --root="$pkgdir" --optimize=1
  install -Dm644 LICENSE.txt "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}

# vim:set ts=2 sw=2 et:
