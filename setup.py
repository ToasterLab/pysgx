import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name='pysgx',
    version='1.0.0',
    scripts=['sgx'],
    author="Huey Lee",
    author_email="leejinhuey@gmail.com",
    description="A wrapper for SGX's unofficial API",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/hueyy/pysgx",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
         "License :: OSI Approved :: MIT License",
         "Operating System :: OS Independent",
    ],

)
