#include <ql/experimental/variancegamma/fftengine.hpp>
#include <ql/experimental/variancegamma/fftvanillaengine.hpp>
#include <ql/stochasticprocess.hpp>
#include <ql/experimental/math/fastfouriertransform.hpp>
#include <ql/math/array.hpp>
#include <complex>
#include <vector>
#include "ffttest.hpp"
#include "customutilities.hpp"

void FastFourierTransformTest::testFFT(){
	
	//boost::shared_ptr<StochasticProcess1D> process;
	//Real logStrikeSpacing;
	//std::vector<boost::shared_ptr<Instrument> > optionList;
	//FFTEngine fft(new FFTEngine(process, logStrikeSpacing));
	//fft.precalculate(optionList);
	//fft.calculate();
	//fft.update();

}


void FastFourierTransformTest::testSimple() {
	BOOST_MESSAGE("Testing complex direct FFT...");
	typedef std::complex<Real> cx;
	cx a[] = { cx(0,0), cx(1,1), cx(3,3), cx(4,4),
		cx(4,4), cx(3,3), cx(1,1), cx(0,0) };
	for(size_t i=0; i<LENGTH(a); i++){
		std::cout << a[i] << std::endl;
	}
	cx b[8];
	FastFourierTransform fft(3);
	fft.transform(a, a+8, b);
	std::cout << std::endl;
	for(size_t i=0; i<LENGTH(b); i++){
		std::cout << b[i] << std::endl;
	}
	cx expected[] = { cx(16,16), cx(-4.8284,-11.6569),
		cx(0,0),   cx(-0.3431,0.8284),
		cx(0,0),   cx(0.8284, -0.3431),
		cx(0,0),   cx(-11.6569,-4.8284) };
	for (size_t i = 0; i<8; i++) {
		if ((std::fabs(b[i].real() - expected[i].real()) > 1.0e-2) ||
			(std::fabs(b[i].imag() - expected[i].imag()) > 1.0e-2))
			BOOST_ERROR("Convolution(" << i << ")\n"
			<< std::setprecision(4) << QL_SCIENTIFIC
			<< "    calculated: " << b[i] << "\n"
			<< "    expected:   " << expected[i]);
		std::cout << "Convolution(" << i << ")\n"
			<< std::setprecision(4) << QL_SCIENTIFIC
			<< "    calculated: " << b[i] << "\n"
			<< "    expected:   " << expected[i] << std::endl;
	}
}

void FastFourierTransformTest::testInverse() {
	BOOST_MESSAGE("Testing convolution via inverse FFT...");
	Array x(3);
	x[0] = 1;
	x[1] = 2;
	x[2] = 3;

	size_t order = FastFourierTransform::min_order(x.size())+1;
	FastFourierTransform fft(order);
	size_t nFrq = fft.output_size();
	std::vector<std::complex<Real> > ft(nFrq);
	std::vector<Real> tmp(nFrq);

	fft.inverse_transform(x.begin(), x.end(), ft.begin());
	std::transform(ft.begin(), ft.end(), tmp.begin(), std::norm<Real>);
	std::fill(ft.begin(), ft.end(), std::complex<Real>());
	fft.inverse_transform(tmp.begin(), tmp.end(), ft.begin());

	// 0
	Real calculated = ft[0].real() / nFrq;
	Real expected = x[0]*x[0] + x[1]*x[1] + x[2]*x[2];
	if (fabs (calculated - expected) > 1.0e-10)
		BOOST_ERROR("Convolution(0)\n"
		<< std::setprecision(16) << QL_SCIENTIFIC
		<< "    calculated: " << calculated << "\n"
		<< "    expected:   " << expected);
	std::cout << "Convolution(0)\n"
		<< std::setprecision(16) << QL_SCIENTIFIC
		<< "    calculated: " << calculated << "\n"
		<< "    expected:   " << expected << std::endl;

	// 1
	calculated = ft[1].real() / nFrq;
	expected = x[0]*x[1] + x[1]*x[2];
	if (fabs (calculated - expected) > 1.0e-10)
		BOOST_ERROR("Convolution(1)\n"
		<< std::setprecision(16) << QL_SCIENTIFIC
		<< "    calculated: " << calculated << "\n"
		<< "    expected:   " << expected);
	std::cout << "Convolution(1)\n"
		<< std::setprecision(16) << QL_SCIENTIFIC
		<< "    calculated: " << calculated << "\n"
		<< "    expected:   " << expected << std::endl;

	// 2
	calculated = ft[2].real() / nFrq;
	expected = x[0]*x[2];
	if (fabs (calculated - expected) > 1.0e-10)
		BOOST_ERROR("Convolution(2)\n"
		<< std::setprecision(16) << QL_SCIENTIFIC
		<< "    calculated: " << calculated << "\n"
		<< "    expected:   " << expected);
	std::cout << "Convolution(2)\n"
		<< std::setprecision(16) << QL_SCIENTIFIC
		<< "    calculated: " << calculated << "\n"
		<< "    expected:   " << expected << std::endl;
}