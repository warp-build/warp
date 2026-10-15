import './App.css'

const features = [
  {
    number: '01',
    title: 'A graph, not a script',
    description:
      'Describe the work and its dependencies. Warp schedules independent actions together and only revisits the parts that changed.',
    mark: '↗',
  },
  {
    number: '02',
    title: 'Content is the contract',
    description:
      'Inputs are fingerprinted from their contents. Identical work can be restored from a local content-addressed cache.',
    mark: '◈',
  },
  {
    number: '03',
    title: 'Use the tools you know',
    description:
      'Warp coordinates language toolchains instead of replacing them. Cargo builds Rust; Bun installs and builds the web.',
    mark: '⌘',
  },
]

function App() {
  return (
    <main>
      <header className="site-header page-width">
        <a className="wordmark" href="#top" aria-label="Warp home">
          <span className="wordmark-icon" aria-hidden="true">w</span>
          <span>warp</span>
        </a>
        <nav className="main-nav" aria-label="Main navigation">
          <a href="#how-it-works">How it works</a>
          <a href="#quickstart">Quickstart</a>
        </nav>
        <a
          className="header-link"
          href="https://github.com/leostera/warp"
          target="_blank"
          rel="noreferrer"
        >
          GitHub <span aria-hidden="true">↗</span>
        </a>
      </header>

      <section className="hero page-width" id="top">
        <div className="hero-copy">
          <p className="eyebrow"><span className="eyebrow-dot" /> A build system for the work ahead</p>
          <h1>Make every build<br />feel like the <em>first.</em></h1>
          <p className="hero-description">
            Warp turns your workspace into a live, incremental graph. The right
            work runs in parallel. The work you already did stays done.
          </p>
          <div className="hero-actions">
            <a className="button button-primary" href="#quickstart">
              Get started <span aria-hidden="true">→</span>
            </a>
            <a className="button button-quiet" href="#how-it-works">
              See how it works <span aria-hidden="true">↓</span>
            </a>
          </div>
          <div className="hero-note">
            <span className="note-line" />
            <span>Rust today. Bun in the same graph.</span>
          </div>
        </div>

        <div className="graph-stage" aria-label="A sample Warp build graph">
          <div className="graph-stamp">ONE WORKSPACE <span>·</span> MANY TOOLCHAINS</div>
          <div className="graph-window">
            <div className="window-bar">
              <span className="window-lights"><i /><i /><i /></span>
              <span>build.graph</span>
              <span className="live-state"><i /> LIVE</span>
            </div>
            <div className="graph-canvas">
              <svg className="graph-lines" viewBox="0 0 500 330" aria-hidden="true">
                <path d="M118 75 C185 75 164 154 233 154" />
                <path d="M118 250 C185 250 164 171 233 171" />
                <path d="M320 162 C374 162 354 83 408 83" />
                <path d="M320 170 C374 170 354 250 408 250" />
                <circle cx="233" cy="162" r="4" />
              </svg>
              <div className="graph-node node-source">
                <span className="node-icon source-icon">⌘</span>
                <span><small>PACKAGE</small><strong>warp-engine</strong></span>
                <span className="node-state done">✓</span>
              </div>
              <div className="graph-node node-site">
                <span className="node-icon site-icon">◒</span>
                <span><small>PACKAGE</small><strong>warp.leostera.dev</strong></span>
                <span className="node-state done">✓</span>
              </div>
              <div className="graph-node node-rust">
                <span className="node-icon rust-icon">R</span>
                <span><small>RUSTC</small><strong>compile</strong></span>
                <span className="node-state cached">CACHED</span>
              </div>
              <div className="graph-node node-bun">
                <span className="node-icon bun-icon">B</span>
                <span><small>BUN</small><strong>vite build</strong></span>
                <span className="node-state done">✓</span>
              </div>
              <div className="graph-caption"><span className="caption-dot" /> Independent work, running together</div>
            </div>
            <div className="window-footer">
              <span>2 ACTIONS</span>
              <span>1.42s ELAPSED</span>
              <span className="footer-result">● ALL GREEN</span>
            </div>
          </div>
          <span className="stage-orbit orbit-one" />
          <span className="stage-orbit orbit-two" />
          <span className="stage-spark spark-one">✳</span>
          <span className="stage-spark spark-two">✳</span>
        </div>
      </section>

      <section className="proof-strip" aria-label="Warp build principles">
        <div className="page-width proof-inner">
          <span>NOT ANOTHER COMPILER</span>
          <span className="proof-rule" />
          <span>YOUR TOOLS, COORDINATED</span>
          <span className="proof-rule" />
          <span>BUILT AROUND REAL INPUTS</span>
        </div>
      </section>

      <section className="features page-width" id="how-it-works">
        <div className="section-heading">
          <p className="eyebrow">A calmer way to build</p>
          <h2>Do the work once.<br /><span>Know what changed.</span></h2>
          <p className="section-intro">
            A build should be a clear model of your project—not a pile of
            commands hoping for the best.
          </p>
        </div>
        <div className="feature-grid">
          {features.map((feature) => (
            <article className="feature-card" key={feature.number}>
              <div className="feature-topline">
                <span>{feature.number}</span>
                <span className="feature-mark" aria-hidden="true">{feature.mark}</span>
              </div>
              <h3>{feature.title}</h3>
              <p>{feature.description}</p>
            </article>
          ))}
        </div>
      </section>

      <section className="quickstart" id="quickstart">
        <div className="page-width quickstart-inner">
          <div className="quickstart-copy">
            <p className="eyebrow"><span className="eyebrow-dot" /> Your workspace, one command away</p>
            <h2>Start with the<br /><em>work you have.</em></h2>
            <p>
              Warp uses Cargo for Rust and Bun for JavaScript projects. Keep
              your manifests and lockfiles; let Warp connect the steps.
            </p>
            <a className="text-link" href="https://github.com/leostera/warp" target="_blank" rel="noreferrer">
              Read the source <span aria-hidden="true">↗</span>
            </a>
          </div>
          <div className="terminal-card" aria-label="Quickstart commands">
            <div className="terminal-top">
              <span className="terminal-lights"><i /><i /><i /></span>
              <span>terminal</span>
              <span className="terminal-path">~/your-workspace</span>
            </div>
            <div className="terminal-body">
              <p><span className="prompt">$</span> bun install --frozen-lockfile</p>
              <p className="terminal-muted">Lockfile is up to date · dependencies installed</p>
              <p><span className="prompt">$</span> warp build ./sites/warp.leostera.dev</p>
              <p className="terminal-success"><span>✓</span> warp.leostera.dev::build <b>cached</b></p>
              <p><span className="prompt">$</span> cd sites/warp.leostera.dev &amp;&amp; cf deploy</p>
              <span className="terminal-cursor" aria-hidden="true" />
            </div>
            <div className="terminal-bottom"><span>BUILD GRAPH READY</span><span>●</span></div>
          </div>
        </div>
      </section>

      <footer className="site-footer page-width">
        <a className="wordmark" href="#top" aria-label="Warp home">
          <span className="wordmark-icon" aria-hidden="true">w</span>
          <span>warp</span>
        </a>
        <span>Make the next build a little less work.</span>
        <a href="https://github.com/leostera/warp" target="_blank" rel="noreferrer">Open source on GitHub ↗</a>
      </footer>
    </main>
  )
}

export default App
