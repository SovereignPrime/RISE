# Maintainer:  T0ha aka Shvein Anton <t0hashvein@gmail.com>

pkgname=RISE
pkgver=0.0.29
pkgrel=1
pkgdesc="Secure sharing and collaboration system"
arch=('any')
url="http://sovereignprime.com"
license=('GPL2')
depends=('python2-gobject' 'webkitgtk' 'pywebkitgtk')
makedepends=('git' 'erlang>=17.0' 'rebar')
source=($pkgname-$pkgver.tgz::https://github.com/SovereignPrime/$pkgname/archive/v$pkgver.tar.gz)
sha256sums=('3ef1734b240bd3b2d68202031f96cbab63ad4e4e22f66c5c1c3b24cb4d76a6bd')

build() {
  cd "$srcdir/$pkgname-$pkgver"
  make linux
}

package() {
  mkdir -p "$pkgdir/opt/RISE"
  mkdir -p "$pkgdir/etc/profile.d"
  cp -r "$srcdir/$pkgname-$pkgver/rel/rise" "$pkgdir/opt/"
  cp "$srcdir/$pkgname-$pkgver/rel/rise/etc/rise.sh" "$pkgdir/etc/profile.d"
}

